package tools;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;

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
			FileOutputStream outStr = new FileOutputStream(file);
			OutputStreamWriter fw = new OutputStreamWriter(outStr);
			StreamResult result = new StreamResult(fw);
			t.transform(src, result);
		} catch (Exception e) {

			e.printStackTrace();
			System.out.println("sorry: " + e.getMessage());
		}
	}

}