
import  de.tuhh.wb.javagis.data.Modeldata;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Enumeration;
import de.tuhh.wb.javagis.model.VersionSession;
// CVS TEST


public class Test {
    public static void main(String[] args)
    {
	Test test=new Test();
    }

    public Test ()
    {
	try
	    {
		Hashtable env=new Hashtable();
		env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
		env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
		env.put("java.naming.provider.url","localhost");
		
		InitialContext ctx=new InitialContext(env);
		System.out.println("getVersionSession: got InitialContext");
		
		String jndiName="Modell"+".VersionSession";
		System.out.println("JNDI-Name: "+jndiName);
		
		System.out.println("debug.. with reflection");
		
		Object objectHome = ctx.lookup(jndiName);
		System.out.println("did lookup");
		
		Class vshc=objectHome.getClass();
		VersionSession versionSession=(VersionSession)(vshc.getMethod("create",null)).invoke(objectHome,null);
		//				VersionSession versionSession=(VersionSession)home.create();
		System.out.println("got VersionSession");
		System.out.println("number of elements: "+versionSession.getSize());
		
		
		Modeldata modeldata=new Modeldata(versionSession);
		modeldata.info();
	    }
	catch(Exception e)
	    {
		e.printStackTrace();
	    }
    }
    
}
