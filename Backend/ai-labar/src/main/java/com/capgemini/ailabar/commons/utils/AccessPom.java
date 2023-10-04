package com.capgemini.ailabar.commons.utils;

import org.springframework.stereotype.Component;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;

@Component
public final class AccessPom {
    private AccessPom() {}
    public static String showAppVersion() {
        try {
            File pomFile = new File("pom.xml");
            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

            Document document = documentBuilder.parse(pomFile);
            Element rootElement = document.getDocumentElement();
            NodeList versionNodeList = rootElement.getElementsByTagName("version");

            if (versionNodeList.getLength() > 0) {
                Node versionNode = versionNodeList.item(0);
                return "AiLabar " + versionNode.getTextContent();
            } else {
                return "No se encontró la versión en el pom.xml";
            }

        } catch (Exception e) {
            return e.getMessage();
        }
    }
}
