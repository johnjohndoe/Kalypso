import java.util.Hashtable;
import java.io.IOException;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.apache.xerces.parsers.SAXParser;

public class KalypsoXmlContentHandler extends DefaultHandler
{    
    //    private String content;
    private String tableName;  // name of table, that should be filled
    private GisTransferObject transferObject;
    private VectorSet vectorSet;
    private VectorSet agregatedVectorSet;
    private Hashtable tables; // stores the mapping between DB-ID and XML-NO
    private String content; // current XML-Entity-content
    private KalypsoXmlImportListener importListener;
    public KalypsoXmlContentHandler(KalypsoXmlImportListener importListener)
    {
	this.importListener=importListener;
	this.tableName=null;
	this.transferObject=null;
	this.vectorSet=null;
	this.agregatedVectorSet=null;
	this.tables=new Hashtable();
	this.content="";
	//	namePath=new NamePath();
    }

    public  void startDocument() 
    {
	System.out.println("start importing KalypsoXml....");
    }

    public void startElement(String uri, String localName, String qName, Attributes atts)
    {
	//	namePath.add(localName);
	//	System.out.println("entering "+namePath.toString());
	// new table...
	if("table".equals(localName))
	    {
		String tableName=atts.getValue("key");
		this.tableName=tableName;
	    }
	// new object...
	else if("o".equals(localName))
	    {
		String id=atts.getValue("ID");
		String no=atts.getValue("NO");
		if(id==null && no==null)
		    System.err.println("<o>can't parse a object to/from database without ID and NO :-(\nObject will stay unparsed");
		if(id!=null && no!=null)
		    System.err.println("<o>sorry can't parse this object to/from database  :-(\n please use ID or NO, not both,\n Object will stay unparsed");
		if(id!=null && no==null)
		    transferObject=new GisTransferObject(tableName,id,true);
		if(id==null && no!=null)
		    transferObject=new GisTransferObject(tableName,no,false);
	    }
	// new simpleProperty...
	else if("sp".equals(localName))
	    {
		if(transferObject==null)
		    {
			System.err.println("can not parse <sp>-tag outside of <o>-tag\n unparsed values"+atts.toString());
		    }
		transferObject.setSimpleProperties(atts);
	    }
	// relation-information...
	else if("rel".equals(localName))
	    {
		if(transferObject==null)
		    {
			System.err.println("can not parse a relation  outside of object (<o>-tag)\n unparsed relation"+atts.toString());
		    }
		transferObject.setRelation(atts);
	    }
	else if("v".equals(localName))
	    {
		if(transferObject==null)
		    {
			System.err.println("can not parse <v>-tag outside of <o>-tag\n unparsed vectorSet");
		    }
		else if(vectorSet==null)
		    {
			vectorSet=new VectorSet(atts);
		    }		    
		else
		    {
			// we are allready inside a vectorSet
			agregatedVectorSet=new VectorSet(atts);
		    }
	    }
	else if("v_row".equals(localName))
	    {
		if(vectorSet==null)
		    {
			System.err.println("can not parse <v_row>-tag outside of vectorSet");
		    }
		else if(agregatedVectorSet!=null)
		    {
			agregatedVectorSet.addRow(atts);
		    }		    
		else
		    {
			vectorSet.addRow(atts);
		    }
	    }
	content="";
    }
    
    public void characters(char[] ch, int start, int end) 
    {
	String string=new String(ch,start,end);
	content=content+string;
    }
    
    public void endElement(String uri, String localName, String qName) 
    {
	if("o".equals(localName))
	    {
		importListener.importObject(transferObject);
		transferObject=null;
	    }
	else if("table".equals(localName))
	    {
		tableName=null;
	    }
	else if("sp".equals(localName))
	    {
		// nix
	    }
	else if("v_row".equals(localName))
	    {
		// nix
	    }
	else if("v".equals(localName))
	    {
		if(agregatedVectorSet!=null)
		    {
			vectorSet.addVectorSet(agregatedVectorSet);
			agregatedVectorSet=null;
		    }
		else 
		    {
			transferObject.addVectorSet(vectorSet);
			vectorSet=null;
		    }
	    }
	content="";
    }
}
