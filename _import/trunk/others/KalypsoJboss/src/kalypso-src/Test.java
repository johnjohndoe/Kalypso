import de.tuhh.wb.javagis.xml.GisTransferObject;
import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
import de.tuhh.wb.javagis.xml.VectorSet;
import de.tuhh.wb.javagis.xml.XmlConvert;
public class Test implements KalypsoXmlImportListener
{

    public static void main(String[] args)
    {
	String zahlen[][]={
	    {"eins","zwei"},
	    {"drei"," vier","fuenf"},
	    {},{},
	    {"sechs"}

	};
	for(int i=0;i<zahlen.length;i++)
	    {
		String[] values=zahlen[i];
		for(int j=0;j<values.length;j++)
		    System.out.print("x"+values[j]);
		System.out.println("");
	    }
	Test test=new Test(args[0]);	
    }

    public Test(String inputFile)
    {
	//	XmlImport parse=new XmlImport(inputFile,(KalypsoXmlVectorSetListener)this);
	String v="<v key=\"m_channelData\"><v_row v_q=\"200.0\" v_kRiver=\"0.06\" v_nRiver=\"1.0\" v_kLand=\"0.0\" v_nLand=\"0.0\"/><v_row v_q=\"400.0\" v_kRiver=\"0.06\" v_nRiver=\"1.0\" v_kLand=\"0.0\" v_nLand=\"0.0\"/><v_row v_q=\"700.0\" v_kRiver=\"0.06\" v_nRiver=\"1.0\" v_kLand=\"0.0\" v_nLand=\"0.0\"/><v_row v_q=\"7000.0\" v_kRiver=\"0.06\" v_nRiver=\"1.0\" v_kLand=\"10.0\" v_nLand=\"4.0\"/><v_row v_q=\"8000.0\" v_kRiver=\"0.06\" v_nRiver=\"1.0\" v_kLand=\"10.0\" v_nLand=\"4.0\"/></v>";
	try
	    {
		XmlConvert xmlConvert=new XmlConvert();
		xmlConvert.parse(v);
		System.out.println("<?xml version=\"1.0\" encoding=\"utf-8\"?>");
		System.out.println("<theme key=\"\">");
		VectorSet vsto=	xmlConvert.toVectorSet();
		System.out.println(vsto.toXmlString());
		System.out.println("</theme>");
	    }	
	catch(Exception e)
	    {}
    }

    public void importObject(GisTransferObject gto)
    {
	//	System.out.println("next");
	String xml=gto.toXmlString();
	System.out.println(xml);
    }
}
