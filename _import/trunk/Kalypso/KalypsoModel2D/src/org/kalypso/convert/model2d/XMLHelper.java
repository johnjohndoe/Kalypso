/*
 * Created on 27.01.2005
 */
package org.kalypso.convert.model2d;

import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Text;

/**
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class XMLHelper {
    
    /**
     * creates the textNode -> textContent of the element
     * @param text
     * @return
     */ 
     public static Text createTextNode(Document doc, String text) {
         String tTmp = "";
         if ( text != null) {
             tTmp = text;
         }
         return doc.createTextNode(tTmp);
     }
     
     /**
      * creates attribute
      * @param attName
      * @param value
      * @return
      */
      public static Attr createAttribute(Document doc, String attName, String value) {
          Attr attr = doc.createAttribute(attName);
          attr.setValue(value);
          return attr;
      }
      
      /**
       * creates a new and empty dom document
       */
      public static Document createDocument () {
          Document doc = null;
          javax.xml.parsers.DocumentBuilder builder = null;
          try {
              DocumentBuilderFactory fac = DocumentBuilderFactory.newInstance ();
              fac.setNamespaceAware (true);
              builder = fac.newDocumentBuilder ();
          } catch (Exception ex) {
              System.out.println (ex);
          }
          doc = builder.newDocument ();
          return doc;
      }
}
