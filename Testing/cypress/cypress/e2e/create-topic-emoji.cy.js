describe("Your Cypress Test Suite", () => {
  beforeEach(() => {
    cy.viewport(2048, 1043);

    cy.visit("http://localhost:4200/ailabar");

    cy.get('[formControlName="user"]').type(Cypress.env("CYPRESS_USERNAME"));

    cy.get('[formControlName="password"]').type(
      Cypress.env("CYPRESS_PASSWORD")
    );

    cy.get("#login-login-button").click();

    cy.url().should("include", "/#/topics/topics-list");
  });

  it("should interact with the elements and perform assertions", () => {
    cy.get("#topicsTable").should("be.visible");

    cy.log("Let's create a topic");
    cy.get("#topic-list-add-topic-button").click();
    cy.url().should("include", "/#/topics/topics-create");
    cy.wait(1000);

    cy.log("We selected the topic of EMOJIS");
    cy.get(".images-container div").should("have.length", 4).eq(2).click();

    cy.log("Fill out all the fields");
    cy.get("#step-two-tittle-input").type("Cypress test emoji");

    cy.get("#step-two-date-input").should("be.visible").click();

    cy.get("#step-two-date-input").type("30/12/2024");

    cy.get("#step-two-openAddParticipants-button").click();
    cy.wait(1000);
    cy.get("#add-groups-topic-select-group").click();
    cy.wait(1000);

    cy.get("mat-option").first().click();

    cy.get("#add-groups-topic-save-button").click();

    cy.wait(1000);
    cy.log("We click on 'Save'");
    cy.get("#topics-create-save-button").click();

    cy.get("#topicsTable tbody tr")
      .invoke("map", ($row) => {
        const elementData = {};
        return elementData;
      })
      .as("dataSource");

    cy.get("#topicsTable tbody tr:first-child [id]")
      .invoke("attr", "id")
      .then((id) => {
        if (id) {
          const parts = id.split("-");
          const lastPart = parts[parts.length - 1];
          cy.wait(1000);
          cy.log("We took the survey");
          cy.get(`#topic-list-vote-tablet-button-${lastPart}`).click();
          cy.wait(1000);
          cy.get(".modal input").first().click();
          cy.wait(1000);
          cy.get(`#modal-votation-save-button`).click();
          cy.wait(1000);
          cy.log("We're closing the survey");
          cy.get(`#topic-list-close-tablet-button-${lastPart}`).click();
          cy.wait(1000);
          cy.log("We visualize the results");
          cy.get(`#topic-list-visibility-tablet-button-${lastPart}`).click();
          cy.wait(1000);
          cy.get(`#rating-result-close-button`).click();
          cy.wait(1000);
          cy.log("We deleted the survey");
          cy.get(`#topic-list-delete-tablet-button-${lastPart}`).click();
          cy.wait(1000);
          cy.get(`#confirm-deletion-topic-yes-button`).click();
        } else {
          cy.log('El atributo "id" no está definido en el primer registro.');
        }
      });
  });
});
