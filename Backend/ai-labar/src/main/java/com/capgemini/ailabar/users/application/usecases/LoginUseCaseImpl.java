package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.users.domain.exceptions.LoginException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.LoginUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.nio.charset.StandardCharsets;
import java.security.PrivateKey;
import java.util.*;

@Service
@Transactional(readOnly = true)
public class LoginUseCaseImpl implements LoginUseCase {
    private final UsersRepositoryPort usersRepositoryPort;
    private final Environment environment;
    @Value("${login.cap.url}")
    private String loginCapUrl;

    public LoginUseCaseImpl(UsersRepositoryPort usersRepositoryPort, Environment environment) {
        this.usersRepositoryPort = usersRepositoryPort;
        this.environment = environment;
    }

    @Override
    public List<String> login(UsersModel usersModel, PrivateKey privateKey) {
        if (usersModel.getUser().isBlank() || usersModel.getPassword().isBlank()) {
            throw new LoginException("User and password are required to login");
        }

        String token = null;

        if("true".equals(environment.getProperty("login.cap.active"))) {
            token = loginCap(usersModel);
        } else {
            if (Boolean.FALSE.equals(usersRepositoryPort.login(usersModel.getUser(), DigestUtils.sha256Hex(usersModel.getPassword())))) {
                throw new LoginException("Login failed");
            }
        }

        UsersEntity usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        if(usersEntity == null) {
            throw new LoginException("User not found");
        }

        List<String> loginData = new ArrayList<>();
        if("true".equals(environment.getProperty("login.cap.active"))) {
            loginData.add(token);
        } else {
            loginData.add(usersEntity.getToken());
        }
        loginData.add(usersEntity.getVisualization());
        loginData.add(usersEntity.getLanguage());
        loginData.add(usersEntity.getPhoto());

        return loginData;
    }

    private String loginCap(UsersModel usersModel) {
        JSONObject jsonObject = requestToCapgemini(usersModel);

        if(!usersRepositoryPort.checkUser(usersModel.getUser())) {
            createCapgeminiUser(usersModel, jsonObject);
        }

        String token = jsonObject.getString("token");

        UsersEntity usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());
        usersRepositoryPort.updateToken(usersEntity.getId(), token);

        return token;
    }

    private JSONObject requestToCapgemini(UsersModel usersModel) {
        RestTemplate restTemplate = new RestTemplate();

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        String jsonBody = "{\"username\":\"" + usersModel.getUser() + "\",\"password\":\"" + usersModel.getPassword() + "\"}";

        HttpEntity<String> requestEntity = new HttpEntity<>(jsonBody, headers);

        try {
            ResponseEntity<String> responseEntity = restTemplate.exchange(
                    loginCapUrl,
                    HttpMethod.POST,
                    requestEntity,
                    String.class
            );

            return new JSONObject(responseEntity.getBody());
        } catch (Exception e) {
            if (e.getMessage().contains("UnknownHostException")) {
                throw new LoginException("It is necessary to connect to Capgemini's VPN");
            }
            if(e.getMessage().startsWith("403")) {
                throw new LoginException("Login failed");
            }
            throw new LoginException("Error with the Capgemini login");
        }
    }

    private void createCapgeminiUser(UsersModel usersModel, JSONObject jsonObject) {
        JSONObject decodedClaims = readJWT(jsonObject);

        // It has been determined that the Capgemini password should not be saved; a password that is never used is generated for company users
        String hashedPassword = DigestUtils.sha256Hex((Math.random()*100+1) + usersModel.getUser() + (Math.random()*100+1));

        UsersEntity usersEntity = new UsersEntity(usersModel);
        usersEntity.setPassword(hashedPassword);
        usersEntity.setToken(jsonObject.getString("token"));
        usersEntity.setEmail(decodedClaims.getString("email"));
        usersEntity.setGender("M");
        usersEntity.setLanguage(usersModel.getLanguage());
        usersEntity.setRegistrationDate(DateTime.actualDateAndTime());
        usersRepositoryPort.createUser(usersEntity);
    }

    private JSONObject readJWT(JSONObject jsonObject) {
        String jwt = jsonObject.getString("token");

        String[] parts = jwt.split("\\.");

        if (parts.length == 3) {
            String header = parts[0];
            String claims = parts[1];
            String signature = parts[2];

            return new JSONObject(decodeBase64Url(claims));
        } else {
            throw new LoginException("The token format is not as expected");
        }
    }

    private String decodeBase64Url(String input) {
        byte[] decodedBytes = Base64.getUrlDecoder().decode(input);
        return new String(decodedBytes, StandardCharsets.UTF_8);
    }
}
