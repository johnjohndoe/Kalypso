import java.io.IOException;
import org.xml.sax.SAXException;
import java.util.Vector;
import java.util.Enumeration;

public class Test implements KalypsoXmlImportListener
{
    public Test()
    {
	XmlImport xmlImport=new XmlImport("test3.xml",this);
	try
	    {
		xmlImport.start();
	    }	    
	catch(IOException e) //zB file not found
	    {
		System.out.println("IOException:"+e.getMessage());
	    }
	catch(SAXException e) //zB kein XML-Format
	    {
 		System.out.println("SAXException:"+e.getMessage());
	    }
    }

    public static void main(String[] args)
    {
	Test test=new Test();
    }

    public void importObject(GisTransferObject gisTransferObject)
    {
	System.out.println("-------------------------------------------------------");
	System.out.println("got new Object from XML ...");
	info(gisTransferObject);
    }    
    
    public void info(GisTransferObject gto)
    {
	System.out.println("-------------------------------------------------------");
	System.out.println("GisTransferObject:");
	System.out.println(" table: "+gto.getTableName());
	System.out.println(" ID:  #"+gto.getIdentifier());
	if(gto.isRelation())
	    {
		System.out.println("  this is a Relation:");
		System.out.print("   from "+gto.getRelationSrcTable()+" #"+gto.getRelationSrcIdentifier());
		System.out.println(" to "+gto.getRelationDestTable()+" #"+gto.getRelationDestIdentifier()+"\n");
	    }
	
	System.out.print(" SimpleProperties:");
	Vector simplePropKeys=gto.getSimplePropertyKeys();
	for(int i=0;i<simplePropKeys.size();i++)
	    {
		String key=(String)simplePropKeys.elementAt(i);
		System.out.print(" "+key+"=\""+gto.getSimpleProperty(key)+"\"");
	    }
	System.out.println();


	// VectorSet
	Vector vectorSetKeys=gto.getVectorSetKeys();
	for(int i=0;i<vectorSetKeys.size();i++)
	    {
		String key=(String)vectorSetKeys.elementAt(i);
		VectorSet vectorSet=gto.getVectorSet(key);
		info4vectorSet("  ",vectorSet);
	    }
    }
    
    public void info4vectorSet(String indent,VectorSet vectorSet)
    {
	System.out.println(indent+"VectorSet \""+vectorSet.getKey()+"\"");
	for(int row=0;row<vectorSet.size();row++)
	    {
		System.out.println(indent+"   row#"+row+":");
		System.out.print(indent+  "       simpleProperties: ");
		Vector v_simplePropKeys=vectorSet.getSimplePropertyKeys(row);
		for(int v_sp=0;v_sp<v_simplePropKeys.size();v_sp++)
		    {
			String v_key=(String)v_simplePropKeys.elementAt(v_sp);
			System.out.print(" "+v_key+"=\""+vectorSet.getSimpleProperty(v_key,row)+"\"");
		    }			
		System.out.println();
		//agregated VectorSets:
		
		Vector agregatedVectorSetKeys=vectorSet.getVectorSetKeys();
		for(int i=0;i<agregatedVectorSetKeys.size();i++)
		    {
			String key=(String)agregatedVectorSetKeys.elementAt(i);
			VectorSet agregatedVectorSet=vectorSet.getVectorSet(key,row);
			info4vectorSet(indent+"       | ",agregatedVectorSet);
		    }			
	    }
	System.out.println(indent+"---------");
    }
}
