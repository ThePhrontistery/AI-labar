package com.capgemini.ailabar.topics.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.application.services.TopicsService;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/topics")
public class TopicsController implements SpecialResponseInterface {
    private final TopicsService topicsService;

    @Autowired
    public TopicsController(TopicsService topicsService) {
        this.topicsService = topicsService;
    }

    /*
     * SE DEVUELVE EL NÚMERO DE TOPICS ESPECIFICADOS ORDENADOS DE MÁS RECIENTES A MÁS ANTIGUOS:
     * 1. Devuelve los topics de los que el user es author (creador) y de los que forma parte de members (autorizado para votar).
     * 2. Se añade el campo canVote, cuyo valor será false si el usuario ya ha votado en este topic y true si aún no lo ha hecho.
     * 3. El atributo elements es obligatorio y sirve para indicar cuántos elementos queremos mostrar en la llamada (paginación). Se acepta un valor 0 en elements, pero
     *    se devolverá un entity vacío.
     * 4. El atributo page sirve para devolver la página deseada y no es obligatorio, se devuelve la primera página por defecto en caso de no especificar una. La primera
     *    page será 1, si se envía 0 se tomará como la primera.
     * 5. El atributo status indica si el topic está abierto (1) o cerrado (0).
     * 6. Existe también un campo llamado filters que se trata de un List<String> para filtrar topics y que puede tener los valores “mines”, “opened”, “closed” y/o “votePending”.
     */
    @PostMapping("loadTopics")
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, Object> topicsEntityList = topicsService.loadTopics(usersModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }

    /*
     * CREA UN TOPIC EN LA BBDD:
     * 1. El user se transforma de manera interna en el author del topic.
     * 2. Las opciones y los members del grupo serán enviados como un array de string.
     * 3. Si se quiere asignar un grupo específico se pasaría el nombre del mismo con el atributo groupName en lugar de members.
     * 4. Si se envía members en lugar de un groupName se genera un grupo temporal asignado a este topic (groupName y members no pueden enviarse juntos).
     * 5. Una fecha de cierre no es obligatoria, pero en caso de aplicarla deberá ser mayor a la fecha actual. Los formatos de fecha permitidos son
     *    [yyyy-MM-dd][yyyy/MM/dd][dd-MM-yyyy][dd/MM/yyyy][MM/dd/yyyy][yyyyMMdd] aunque en base de datos se almacenan como yyyyMMdd.
     * 6. El campo Options sólo debe contener image si el type es IMAGE_SINGLE o IMAGE_MULTIPLE.
     */
    @PostMapping("/createTopic")
    public ResponseEntity<SpecialResponse> createTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.createTopic(topicsModel);
        responseJson.put("message", "Topic created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * EDITA UN TOPIC DE LA BBDD:
     * 1. Es igual que createTopic a excepción de que es un PUT y de que el método servirá para editar el topic con el id indicado siempre que el user sea el author del mismo.
     * 2. Si el type o las options del topic son modificadas, se invalidan los votos actuales, reseteándose a 0 y se habilita a los members que habían votado para que puedan volver a hacerlo.
     * 3. Si el type del topic es de tipo IMAGE, es obligatorio enviar el campo image con el valor de la misma en Base64 además del campo option.
     * 4. Si existe un closeDate en la base de datos para el topic y en editTopic no se envía el valor o se envía vacío, la fecha de cierre en la base de datos se elimina.
     */
    @PutMapping("/editTopic")
    public ResponseEntity<SpecialResponse> editTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.editTopic(topicsModel);
        responseJson.put("message", "Topic edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * CIERRA UN TOPIC DE LA BBDD EN ESTADO ABIERTO:
     * 1. Cambia el status del topic con el id enviado a Cerrado (0), por lo que ya sólo podrá ser eliminado o visualizado su resultado.
     */
    @PutMapping("/closeTopic")
    public ResponseEntity<SpecialResponse> closeTopic(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.closeTopic(topicModel);
        responseJson.put("message", "The topic has been closed");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * ABRE UN TOPIC DE LA BBDD EN ESTADO CERRADO:
     * 1. Cambia el status del topic con el id enviado a Abierto (1).
     */
    @PutMapping("/reOpenTopic")
    public ResponseEntity<SpecialResponse> reOpenTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.reOpenTopic(topicsModel);
        responseJson.put("message", "Topic reopened");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * ELIMINA UN TOPIC DE LA BBDD:
     * 1. Elimina el topic con el id indicado, así como sus referencias a tablas intermedias como las opciones de votación, los miembros que han votado y el grupo temporal asignado si lo tuviera.
     */
    @DeleteMapping("/deleteTopic")
    public ResponseEntity<SpecialResponse> deleteTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.deleteTopic(topicsModel);
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Método exclusivo para desarrollo */
    /*
     * DEVUELVE TODOS LOS DATOS DE TODOS LOS TOPICS DE LA BBDD (EXCLUSIVO PARA PRUEBAS DE DESARROLLO, NO DEBE IR EN LA VERSIÓN FINAL)
     */
    @GetMapping("/getTopicsDatabase")
    public ResponseEntity<SpecialResponse> getTopicsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<TopicsModel> topicsEntityList = topicsService.getTopicsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }
    /* Fin del método únicamente para desarrollo */

    /*
     * SE UTILIZA PARA REGISTRAR LA VOTACIÓN DEL USUARIO:
     * 1. El método suma 1 a la opción u opciones seleccionadas por el usuario siempre que este no haya votado anteriormente.
     * 2. Es importante que las opciones, aunque sean únicas, se envíen como un array de strings.
     * 3. Si las opciones de votación enviadas no coinciden con las disponibles en el topic (no debería suceder), devolverá un error.
     */
    @PutMapping("/vote")
    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.vote(topicsModel);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DEVUELVE LOS RESULTADOS DE LAS VOTACIONES:
     * 1. Devuelve las opciones y los votaciones asignadas a las mismas siempre y cuando el topic esté en estado Cerrado (0).
     * 2. Si el type del topic es AS, se devolverá también el campo image con la foto del usuario en la base de datos si existe el usuario y tiene foto.
     */
    @PostMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, List<OptionsModel>> mapTopicTypeAndOptionsModelList = topicsService.votingResults(topicsModel);
        String topicType = mapTopicTypeAndOptionsModelList.keySet().stream().findFirst().orElse(null);
        List<OptionsModel> optionsModelList = mapTopicTypeAndOptionsModelList.values().stream().findFirst().orElse(null);
        responseJson.put("message", topicType);
        return new ResponseEntity<>(specialResponse(optionsModelList, responseJson), HttpStatus.OK);
    }

    // Manejo de las excepciones de cada caso de uso
    @ExceptionHandler(LoadTopicException.class)
    ResponseEntity<SpecialResponse> handlerLoadTopicException (LoadTopicException loadTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", loadTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(CreateTopicException.class)
    ResponseEntity<SpecialResponse> handlerCreateTopicException (CreateTopicException createTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", createTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(EditTopicException.class)
    ResponseEntity<SpecialResponse> handlerEditTopicException (EditTopicException editTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", editTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(CloseTopicException.class)
    ResponseEntity<SpecialResponse> handlerCloseTopicException (CloseTopicException closeTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", closeTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(ReOpenTopicException.class)
    ResponseEntity<SpecialResponse> handlerReOpenTopicException (ReOpenTopicException reOpenTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", reOpenTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(VoteTopicException.class)
    ResponseEntity<SpecialResponse> handlerVoteTopicException (VoteTopicException voteException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", voteException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(VotingResultsTopicException.class)
    ResponseEntity<SpecialResponse> handlerVotingResultsTopicException (VotingResultsTopicException votingResultsTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", votingResultsTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(MailServiceException.class)
    ResponseEntity<SpecialResponse> handlerMailServiceException (MailServiceException mailServiceException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", mailServiceException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
