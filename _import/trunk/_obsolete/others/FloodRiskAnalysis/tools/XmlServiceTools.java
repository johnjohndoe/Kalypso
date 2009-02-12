package tools;

import java.io.File;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Node;

public class XmlServiceTools {

	/**
	 * writes Node to xml-file
	 * 
	 * @param file
	 * @param node
	 */
	public static void toFile(File file, Node node) {
		try {
			Transformer t = TransformerFactory.newInstance().newTransformer();
			DOMSource src = new DOMSource(node);
      Result result = new StreamResult( file );
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
      t.setOutputProperty( OutputKeys.INDENT, "yes" );
			t.transform(src, result);
		} catch (Exception e) {

			e.printStackTrace();
			System.out.println("sorry: " + e.getMessage());
		}
	}

}