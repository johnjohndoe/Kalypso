package org.kalypso.robotronadapter;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.NameNotFoundException;
import javax.naming.NamingEnumeration;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;

public class Test
{
  public Test()
  {
    Hashtable env = new Hashtable();
    // set the parameters for the intial context
    env.put( Context.INITIAL_CONTEXT_FACTORY,
        "com.sun.jndi.ldap.LdapCtxFactory" );
    env.put( Context.PROVIDER_URL,
        "ldap://172.16.0.252:6666/dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" );
    env.put( Context.SECURITY_PRINCIPAL,
        "cn=admin,dc=hvz,dc=lhw,dc=mlu,dc=lsa-net,dc=de" );
    env.put( Context.SECURITY_CREDENTIALS, "geheim" );

    try
    {
      // create the initial context
      DirContext rootCtx = new InitialDirContext( env );

      try
      {
        // define the attributes you want to retrieve
        String[] userAttNames =
        {
            "gruppe",
            "userPassword" };
        // perform a user search
        String userName = "kmustermann";
        Attributes userAtts = rootCtx.getAttributes( "uid=" + userName
            + ",ou=benutzer", userAttNames );
        System.out.println( "user [" + userName + "] exists" );
        // read the retrieved attribute values
        String groupName = (String)userAtts.get( "gruppe" ).get();
        System.out.println( "\tgruppe: " + groupName );
        // check the password
        // ...

        // perform a search to retrieve the rights for the users group
        String[] rightAttNames =
        { "recht" };
        Attributes rightsAtt = rootCtx.getAttributes( "cn=" + groupName
            + ",ou=gruppen" );
        // read the retrieved rights
        NamingEnumeration rightsEnum = rightsAtt.get( "recht" ).getAll();
        while( rightsEnum.hasMore() )
        {
          String right = rightsEnum.next().toString();
          System.out.println( "\trecht: " + right );
        }
      }
      catch( NameNotFoundException nnfEx )
      {
        System.out.println( "unknown userID" );
      }
    }
    catch( Exception ex )
    {
      ex.printStackTrace();
    }
  }

  public static void main( String[] args )
  {
    Test si = new Test();
  }
}
