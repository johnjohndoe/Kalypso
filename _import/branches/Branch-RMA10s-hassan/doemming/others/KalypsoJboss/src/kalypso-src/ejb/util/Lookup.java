package ejb.util;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import java.util.Properties;

import javax.rmi.PortableRemoteObject;


public final class Lookup {

    private static Context rootContext;

    static {
	//        Properties p = new Properties();
	//        p.put(Context.INITIAL_CONTEXT_FACTORY, "weblogic.jndi.WLInitialContextFactory");
	//        p.put(Context.PROVIDER_URL, "t3://localhost:7001");
	Hashtable env=new Hashtable();

	env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
	env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
	env.put("java.naming.provider.url","127.0.0.1");
	//	env.put("java.naming.rmi.security.manager", "yes");
        try {
	    //            rootContext = new InitialContext(p);
	    rootContext = new InitialContext(env);
        } catch(Exception ex) {
            ex.printStackTrace();
            throw new IllegalStateException(ex.getMessage());
        }
    }


    //No instances required
    private Lookup() { }
    

    public static Object get(String name)
        throws NamingException
    {
        if(name == null) {
            throw new IllegalArgumentException("name must not be null!");
        }
        Object ret = rootContext.lookup(name);
        return ret;
    }


    public static Object narrow(String name, Class c)
        throws NamingException
    {
        if(name == null) {
            throw new IllegalArgumentException("name must not be null!");
        }
        if(c == null) {
            throw new IllegalArgumentException("class must not be null!");
        }

        Object ret = rootContext.lookup(name);
        return PortableRemoteObject.narrow(ret, c); 
    }

}
