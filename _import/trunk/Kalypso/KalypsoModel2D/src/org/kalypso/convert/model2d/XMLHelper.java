/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.
 
 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.
 
 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 katharina.lupp@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
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
