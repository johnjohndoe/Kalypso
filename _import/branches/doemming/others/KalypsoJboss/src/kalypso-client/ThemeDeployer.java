//package de.tuhh.wb.javagis.data;

import java.util.Hashtable;
import javax.naming.InitialContext;
import javax.rmi.PortableRemoteObject;

import de.tuhh.wb.javagis.model.ThemeManagerHome;

//import de.tuhh.wb.javagis.model.na.VersionSessionHome;
//import de.tuhh.wb.javagis.model.control.VersionSessionHome;

public class ThemeDeployer
{
    private Hashtable env;

    public static void main(String[] args)
    {
	try
	    {
		ThemeDeployer themeDeployer=new ThemeDeployer(args[0]);
	    }
	catch(Exception e)
	    {
		System.out.println("Exception: "+e);
	    }
    }

    public ThemeDeployer(String host) throws Exception
    {

	this.env=new Hashtable();
	env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
	env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
	//	env.put("java.naming.provider.url","localhost");
	env.put("java.naming.provider.url",host);
	System.out.println("Host: "+host);
	InitialContext ctx=new InitialContext(env);
	System.out.println("got InitialContext");

	Object objectHome= ctx.lookup("ejb/ThemeManager");
	System.out.println("did lookup");        

	ThemeManagerHome themeHome=(ThemeManagerHome)PortableRemoteObject.narrow(objectHome,ThemeManagerHome.class);
	System.out.println("did narrow");
 
	remove(themeHome);
	create(themeHome);
    }

    public void create(ThemeManagerHome themeHome) throws Exception
    {
	themeHome.create("Test1",de.tuhh.wb.javagis.model.test1.VersionSessionHome.class,"Test1.VersionSession");
	themeHome.create("Test2",de.tuhh.wb.javagis.model.test2.VersionSessionHome.class,"Test2.VersionSession");
	System.out.println("did create");
    }

    public void remove(ThemeManagerHome themeHome) throws Exception
    {
	themeHome.remove("Test1");
	themeHome.remove("Test2");
	System.out.println("did remove");
    }
}
