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
package org.kalypso.convert.model2d;

import java.io.File;

import org.kalypso.calc2d.CalcJob2dModel;
import org.kalypso.calc2d.Constants2D;
import org.kalypso.services.calculation.service.CalcJobServiceException;

/**
 * this class converts the femXMLFile...
 * 
 * Created on 30.08.2004
 * 
 * @author Katharina Lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 */
public class Test
{

  public static void main( String[] args ) throws CalcJobServiceException
  {

//    File baseDir = new File( "C:/Programme/eclipse/workspace/Kalypso2d" );
    File baseDir = new File( Constants2D.BASE_DIR_NAME );
    System.out.println("baseDir: " + baseDir);

    CalcJob2dModel calc = new CalcJob2dModel();
    calc.run( baseDir, null );

  }

  /*
   * public static void main(String[] args) { MeshPoint mp1 = new MeshPoint(143,
   * 3586257.8984375, 5936190.1484375, 63.89112854003906); // MeshPoint mp2 =
   * new MeshPoint(144, 3586342.0546875, 5936223.4296875, 63.74031066894531);
   * MeshPoint mp3 = new MeshPoint(298, 3586248.7890625, 5936184.33984375,
   * 64.0420150756836); MeshPoint mp4 = new MeshPoint(299, 3586256.203125,
   * 5936158.1875, 63.40810012817383);
   *  // Edge edge1 = new Edge(518, 299, 298, 226, 225); // Edge edge2 = new
   * Edge(521, 298, 143, 226, 521); // Edge edge3 = new Edge(246, 143, 144, 226,
   * 105); // Edge edge4 = new Edge(522, 144, 299, 226, 293);
   * 
   * double[]a = new double[]{ mp4.getX()-mp3.getX(), mp4.getY()-mp3.getY(), 0d };
   * double[]b = new double[]{ mp1.getX()-mp3.getX(), mp1.getY()-mp3.getY(), 0d };
   * 
   * //test2 // double[]c = new double[]{ // mp4.getX()-mp2.getX(), //
   * mp4.getY()-mp2.getY(), // 0d // }; // double[]d = new double[]{ //
   * mp1.getX()-mp2.getX(), // mp1.getY()-mp2.getY(), // 0d // };
   * 
   * boolean existsClockwise = GeometryHelper.isAngleClockwise(a, b);
   * System.out.println("test "+ existsClockwise); boolean existsClockwise2 =
   * GeometryHelper.isAngleClockwise(b, a); System.out.println("test "+
   * existsClockwise2); // boolean existsNotClockwise =
   * edge.existEdgeNotClockwise(this.edgesList,idFE); }
   */
}

/**
 * @param args
 * @throws MalformedURLException
 * @throws FileNotFoundException
 * @throws IOException
 * @throws UnsupportedEncodingException
 * @throws SAXException
 *           main: this is the starting point. reads out the arguments form the
 *           command-line
 * @param args
 *          the command-line arguments
 */
//    public static void main(String[] args) {
//
//        HashMap argsmap = null;
//        // no parameter given
//        if (args.length == 0) {
//            System.out.println("AsciiFactory: missing arguments");
//            System.exit(1);
//        } else if (args[0].equals("--help")) {
//            usage(0);
//            System.exit(0);
//        } else if (args.length >= 2) {
//            // two or more parameter
//            argsmap = new HashMap();
//            for (int i = 0; i < args.length; i += 2) {
//                argsmap.put(args[i], args[i + 1]);
//            }
//        }
//
//        if (args[0].equalsIgnoreCase("AsciiFactory"))
//            usage(0);
//        if (argsmap.get("-i") != null && argsmap.get("-o") != null) {
//            HashMap argsmap2 = new HashMap();
//            argsmap2.put("InputFile", argsmap.get("-i"));
//            argsmap2.put("OutFileDirectory", argsmap.get("-o"));
//
//            String inputFile = argsmap2.get("InputFile").toString();
//            System.out.println(inputFile);
//            FileInputStream instream = null;
//            InputSource is = null;
//            try {
//                
//                /*//MyObject o = // get JAXB content tree
//                JAXBContext jc = JAXBContext.newInstance( "com.acme.foo:com.acme.bar" );
//                
//                // jaxbContext is a JAXBContext object from which 'o' is created.
//                JAXBSource source = new JAXBSource( jaxbContext, o );
//                
//                // set up XSLT transformation
//                TransformerFactory tf = TransformerFactory.newInstance();
//                Transformer t = tf.newTransformer(new StreamSource("test.xsl"));
//                
//                // run transformation
//                t.transform(source,new StreamResult(System.out));
//             */
//                
//                
//// SAXParserFactory spf = SAXParserFactory.newInstance();
//// spf.setNamespaceAware(true);
//// XMLReader xmlReader = spf.newSAXParser().getXMLReader();
////
//// xmlReader.setContentHandler(new MyContentHandler());
////
//// //instream = new FileInputStream("books.xml");
//// instream = new FileInputStream(inputFile);
//// is = new InputSource(instream);
//// xmlReader.parse(is);
//                
//                MeshPolygon meshPolygon = new MeshPolygon();
//                meshPolygon.setPoint(1);
//                meshPolygon.setPoint(2);
//                meshPolygon.setPoint(32);
//                meshPolygon.setPoint(22);
//                meshPolygon.setPoint(187);
//                
//                System.out.println("EOF OF TEST");
//            }catch(Exception ex){
//                System.out.println("Error in AsciiFactory... ");
//                ex.printStackTrace();
//            }
//                
//// } catch (IOException e) {
//// e.printStackTrace();
//// System.exit(0);
//// } catch (SAXException e) {
//// e.printStackTrace();
//// System.exit(0);
//// } catch (ParserConfigurationException e) {
//// e.printStackTrace();
//// System.exit(0);
//// }
//
//        }
//    }
//}
