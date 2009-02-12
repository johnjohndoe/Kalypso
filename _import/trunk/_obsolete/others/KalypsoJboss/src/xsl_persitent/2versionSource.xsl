<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text"/>
  <xsl:variable name="package" select="/theme/package"/>
  
  <xsl:template match="/theme">
    <xsl:variable name="themeName">
      <xsl:call-template name="toUpperCase">
        <xsl:with-param name="word" select="@ID"/>
      </xsl:call-template>
    </xsl:variable>

         <!--===============================================================================-->
         <!--                                                                               -->
         <!--                             SessionBean                                       -->
         <!--                                                                               -->
         <!--===============================================================================-->
         
         package <xsl:value-of select="$package"/>;
         
         import javax.naming.InitialContext;
         import javax.rmi.PortableRemoteObject;
 
         import javax.ejb.CreateException;
         import javax.ejb.RemoveException;
         import javax.ejb.FinderException;
         import javax.ejb.EJBException;
         import javax.ejb.SessionBean;
         import javax.ejb.SessionContext;
         import javax.naming.InitialContext;
         import javax.naming.NamingException;
         import java.util.Vector;
         import java.util.Hashtable;
         import java.util.Enumeration;
         import java.util.Collection;
         import java.util.Iterator;



         import de.tuhh.wb.javagis.model.Tools;    
         import de.tuhh.wb.javagis.model.ObjectSession;    
         import de.tuhh.wb.javagis.model.RelationSession;    
         import de.tuhh.wb.javagis.model.ElementSession;
         import de.tuhh.wb.javagis.model.ElementLocal;

         import de.tuhh.wb.javagis.xml.GisTransferObject;
         import de.tuhh.wb.javagis.xml.XmlImport;
         import java.io.PrintWriter;
         import java.io.FileWriter;
         import java.io.File;

         import java.io.File;
         import java.io.IOException;
         import java.util.Properties;
         import java.io.InputStream;
         import java.io.FileInputStream;
         import java.io.FileOutputStream;

         
         /**
         * 
         * @ejb:bean   name="<xsl:value-of select="$themeName"/>.VersionSession"
         *             jndi-name="<xsl:value-of select="$themeName"/>.VersionSession"
         *             local-jndi-name="<xsl:value-of select="$themeName"/>.VersionSession"
         *             view-type="remote"
         *             type="Stateful"
         *             cmp-version="2.x"
         * @ejb:interface extends="de.tuhh.wb.javagis.model.VersionSession"
         * @ejb:home extends="de.tuhh.wb.javagis.model.VersionSessionHome"
<!--         * @jboss:remove-table remove="true" -->
         * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.Version"
         *              view-type="local"
         *              ref-name="ejb/Versions"
         * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.IdManager"
         *              view-type="local"
         *              ref-name="ejb/<xsl:value-of select="$themeName"/>.IdManager"
         <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
           <xsl:variable name="anyClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.<xsl:value-of select="$anyClass"/>"
           *              view-type="local"
           *              ref-name="ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$anyClass"/>"
         </xsl:for-each>
         */       

         public class VersionSessionBean implements SessionBean,de.tuhh.wb.javagis.xml.KalypsoXmlImportListener
         {
         
         private transient VersionLocal importXmlDestinationVersionLocal;
         private transient IdManagerLocal idManagerLocal;
         private transient VersionLocalHome myVersionLocalHome;

         private SessionContext ctx=null;
         
         private transient Hashtable objectIdManagers;
         private transient Hashtable relationIdManagers;
         private transient Hashtable objectLocalHomes;
         private transient Hashtable relationLocalHomes;

         private transient Hashtable importIdMapping;

         /**
         * @ejb:interface-method
         */
         public int getSize() throws javax.ejb.FinderException
         {
          Collection col=myVersionLocalHome.findAll();
          return col.size();
         }

         /**
         * @ejb:interface-method
         */
         public String getVersionName(Object oId) throws FinderException
         {
         return getVersion(oId).getVersionName();
         }
         /**
         * @ejb:interface-method
         */
         public void setVersionName(Object oId,String name) throws FinderException
         {
         getVersion(oId).setVersionName(name);
         }

         /**
         * @ejb:interface-method
         */
         public String getVersionProject(Object oId) throws FinderException
         {
         return getVersion(oId).getVersionProject();
         }

         /**
         * @ejb:interface-method
         */
         public void setVersionProject(Object oId,String project) throws FinderException
         {
         getVersion(oId).setVersionProject(project);
         }

         /**
         * @ejb:interface-method
         */
         public String getVersionState(Object oId) throws FinderException
         {
         return getVersion(oId).getVersionState();
         }

         /**
         * @ejb:interface-method
         */
         public void setVersionState(Object oId,String state) throws FinderException
         {
         getVersion(oId).setVersionState(state);
         }

         /**
         * @ejb:interface-method
         */
         public String getVersionDescription(Object oId) throws FinderException
         {
         return getVersion(oId).getVersionDescription();
         }

         /**
         * @ejb:interface-method
         */
         public void setVersionDescription(Object oId,String description) throws FinderException
         {
         getVersion(oId).setVersionDescription(description);
         }

         /**
         * @ejb:interface-method
         */
         public String getVersionHistory(Object oId) throws FinderException
         {
         return getVersion(oId).getVersionHistory();
         }

         /**
         * @ejb:interface-method
         */
         public void setVersionHistory(Object oId,String history) throws FinderException
         {
         getVersion(oId).setVersionHistory(history);
         }
         
         /**
         * @ejb:interface-method
         */
         public Vector getPrimaryKeyList() throws javax.ejb.FinderException
         {
           Collection col=myVersionLocalHome.findAll();
           Vector primaryKeyList=new Vector();
           System.out.println("VersionSession:  got myObjects ("+col.size()+" elements) ... ");
           Iterator it=col.iterator();
           while(it.hasNext())
           {
             primaryKeyList.add(((VersionLocal) it.next()).getPrimaryKey());
           }
           return primaryKeyList;
         }

         /**
         * @ejb:interface-method
         */
         public Object createVersion(String project,String name,String state,String description,String history) throws CreateException
         {
          Integer newId=idManagerLocal.useId();
          VersionLocal newVersion = myVersionLocalHome.create(newId);
          newVersion.setVersionProject(project);
          newVersion.setVersionState(state);
          newVersion.setVersionName(name);
          newVersion.setVersionDescription(description);
          newVersion.setVersionHistory(history);
          return newId;
         }
         
         /**
         * @ejb:interface-method
         */
         public void removeVersion(Object oId) throws RemoveException, FinderException
         {
          getVersion(oId).remove();
         }
         
         /**
         * Bean-logic
         */
         private VersionLocal getVersion(Object oId) throws FinderException
         {
          VersionLocal version=myVersionLocalHome.findByPrimaryKey((Integer)oId);
          return version;
         }
         
         /**
         * @ejb:interface-method
         */
         public Hashtable objectSessions(Object versionId)
         throws javax.naming.NamingException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
         System.out.println("request elementSessionObjects");
         Hashtable env=new Hashtable();
         env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
         env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
         env.put("java.naming.provider.url","localhost");
         InitialContext ctx=new InitialContext(env);
         
         Hashtable sessions=new Hashtable();
         Object objectHome;
         
         <xsl:for-each select="//objectClass">
           <xsl:variable name="objectClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           objectHome= ctx.lookup("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>Session");
           <xsl:value-of select="$objectClass"/>SessionHome homeOf<xsl:value-of select="$objectClass"/>=(<xsl:value-of select="$objectClass"/>SessionHome)PortableRemoteObject.narrow(objectHome,<xsl:value-of select="$objectClass"/>SessionHome.class);
           System.out.println("got ObjectSessionHome...");
           sessions.put("<xsl:value-of select="@ID"/>",homeOf<xsl:value-of select="$objectClass"/>.create((Integer)versionId));
           System.out.println("got objectSession");
         </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return sessions;
        }

         /**
         * @ejb:interface-method
         */
         public ObjectSession returnObjectSession(Object versionId,String objectSessionKey)
         throws javax.naming.NamingException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          System.out.println("request elementSessionObject "+objectSessionKey);
          Hashtable env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");
          InitialContext ctx=new InitialContext(env);
         
         Object objectHome;
         ObjectSession objectSession=null;
         <xsl:for-each select="//objectClass">
           <xsl:variable name="objectClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           if("<xsl:value-of select="@ID"/>".equals(objectSessionKey))
           {
            objectHome= ctx.lookup("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>Session");
            <xsl:value-of select="$objectClass"/>SessionHome homeOf<xsl:value-of select="$objectClass"/>=(<xsl:value-of select="$objectClass"/>SessionHome)PortableRemoteObject.narrow(objectHome,<xsl:value-of select="$objectClass"/>SessionHome.class);
            System.out.println("got ObjectSessionHome...");
            objectSession =(ObjectSession)homeOf<xsl:value-of select="$objectClass"/>.create((Integer)versionId);
            System.out.println("got objectSession");
           }
          </xsl:for-each> 
         return objectSession;
        }

         /**
         * @ejb:interface-method
         */
         public Hashtable relationSessions(Object versionId)
         throws javax.naming.NamingException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          System.out.println("request relationSessionObjects");
          Hashtable env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");
          InitialContext ctx=new InitialContext(env);
         
          Hashtable sessions=new Hashtable();
          Object objectHome;
         
         <xsl:for-each select="//relationClass">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           objectHome= ctx.lookup("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>Session");
           <xsl:value-of select="$relationClass"/>SessionHome homeOf<xsl:value-of select="$relationClass"/>=(<xsl:value-of select="$relationClass"/>SessionHome)PortableRemoteObject.narrow(objectHome,<xsl:value-of select="$relationClass"/>SessionHome.class);
           System.out.println("got RelationSessionHome...");
           sessions.put("<xsl:value-of select="@ID"/>",homeOf<xsl:value-of select="$relationClass"/>.create((Integer)versionId));
           System.out.println("got relationSession");
         </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return sessions;
        }

         /**
         * @ejb:interface-method
         */
         public RelationSession returnRelationSession(Object versionId,String relationSessionKey)
         throws javax.naming.NamingException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          System.out.println("request relationSessionObject "+relationSessionKey);
          Hashtable env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");
          InitialContext ctx=new InitialContext(env);
         
          RelationSession relationSession=null;
          Object objectHome;
         
         <xsl:for-each select="//relationClass">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           if("<xsl:value-of select="@ID"/>".equals(relationSessionKey))
           {
            objectHome= ctx.lookup("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>Session");
            <xsl:value-of select="$relationClass"/>SessionHome homeOf<xsl:value-of select="$relationClass"/>=(<xsl:value-of select="$relationClass"/>SessionHome)PortableRemoteObject.narrow(objectHome,<xsl:value-of select="$relationClass"/>SessionHome.class);
            System.out.println("got RelationSessionHome...");
            relationSession=(RelationSession)homeOf<xsl:value-of select="$relationClass"/>.create((Integer)versionId);
            System.out.println("got relationSession");
           }
          </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return relationSession;
        }

         /**
         * @ejb:interface-method
         */
         public ElementSession returnElementSession(Object versionId,String elementSessionKey)
         throws javax.naming.NamingException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          System.out.println("request elementSessionObject "+elementSessionKey);
          Hashtable env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");
          InitialContext ctx=new InitialContext(env);
         
          ElementSession elementSession=null;
          Object objectHome;
         
          <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
           <xsl:variable name="elementClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           if("<xsl:value-of select="@ID"/>".equals(elementSessionKey))
           {
            objectHome= ctx.lookup("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$elementClass"/>Session");
            <xsl:value-of select="$elementClass"/>SessionHome homeOf<xsl:value-of select="$elementClass"/>=(<xsl:value-of select="$elementClass"/>SessionHome)PortableRemoteObject.narrow(objectHome,<xsl:value-of select="$elementClass"/>SessionHome.class);
            System.out.println("got ElementSessionHome...");
            elementSession=(ElementSession)homeOf<xsl:value-of select="$elementClass"/>.create((Integer)versionId);
            System.out.println("got relationSession");
           }
          </xsl:for-each> 
         return elementSession;
        }

         /**
         * @ejb:interface-method
         */
         public void exportToXml(Object versionId,String fileName) throws javax.naming.NamingException,
         java.io.IOException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
           Hashtable objectSessions=objectSessions( versionId);
           Hashtable relationSessions=relationSessions( versionId);

           File outFile=new File(getTempDir(),fileName);
           fileName=outFile.getPath();
           PrintWriter out=new PrintWriter(new FileWriter(fileName));
           Tools.genXmlTag(out,"?xml version=\"1.0\" encoding=\"UTF-8\"?");
           Tools.genXmlTag(out,"theme");
           out.close();  // ToDo: use LocalInterfaces to avoid many file-open/closing-actions
          
           for (Enumeration e = objectSessions.elements() ; e.hasMoreElements() ;)
           {
            ElementSession session=(ElementSession)e.nextElement();
            session.toXML(fileName);
           }
           for (Enumeration e = relationSessions.elements() ; e.hasMoreElements() ;)
           {          
            ElementSession session=(ElementSession)e.nextElement();
            session.toXML(fileName);
           }
           out=new PrintWriter(new FileWriter(fileName,true));
           Tools.genXmlTag(out,"/theme");
           out.close();  
         }

         public void setSessionContext(SessionContext ctx)
         {
         this.ctx=ctx;
         }


         /**
         * @ejb:interface-method
         */
         public int getNumberOfObjects(Object vId) throws FinderException
         {
         return getVersion(vId).getNumberOfObjects();
         }


         /**
         * @ejb:create-method
         */
         public void ejbCreate() throws CreateException
         {
          ejbActivate();
         }         

         public void ejbActivate() // prepare for activate
         {
          System.out.println("activate...");

          Hashtable env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");

          try
          {
           importXmlDestinationVersionLocal=null;

             InitialContext ctx=new InitialContext(env);
             myVersionLocalHome =(VersionLocalHome)
             ctx.lookup("java:comp/env/ejb/Versions");
             System.out.println("got VersionLocalHome... ");
             IdManagerLocalHome idManagerLocalHome =(IdManagerLocalHome) ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.IdManager"); 
             System.out.println("got IdManagerLocalHome... ");
             try
             { 
               idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.Version");
             }
             catch(FinderException e)
             {
               System.out.println(e.getMessage());
               System.out.println("maybe IdManager has not been created for this table, trying ....");
               idManagerLocalHome.create("<xsl:value-of select="$themeName"/>.Version");
               idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.Version");
               System.out.println("yes, got it");
             }

           this.objectIdManagers=returnObjectIdManagers();
           this.relationIdManagers=returnRelationIdManagers();
           this.objectLocalHomes=returnObjectLocalHomes();
           this.relationLocalHomes=returnRelationLocalHomes();
          }
          catch(Exception e)
          {
           System.out.println("Exception: "+e.getMessage());
          } 
         }

         public void ejbRemove() // free recources
         {    
          this.importXmlDestinationVersionLocal=null;
          this.idManagerLocal=null;
          this.myVersionLocalHome=null;

          this.objectIdManagers=null;
          this.relationIdManagers=null;
          this.objectLocalHomes=null;
          this.relationLocalHomes=null;

           idManagerLocal=null;
           myVersionLocalHome=null;
           System.out.println("remove VersionSession...");
         }
         
         public void ejbPassivate() // prepare for passivate
         {
          this.importXmlDestinationVersionLocal=null;
          this.idManagerLocal=null;
          this.myVersionLocalHome=null;
 
          this.objectIdManagers=null;
          this.relationIdManagers=null;
          this.objectLocalHomes=null;
          this.relationLocalHomes=null;
          this.importXmlDestinationVersionLocal=null;
          System.out.println("passivate VersionSession...");
         }
         
         

         <!--
         /**
         * @ejb:interface-method
         */
-->
         private Hashtable returnObjectLocalHomes()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
         Hashtable env=new Hashtable();
         env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
         env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
         env.put("java.naming.provider.url","localhost");
         InitialContext ctx=new InitialContext(env);
         
         Hashtable objectLocalHomes=new Hashtable();         

         <xsl:for-each select="//objectClass">
           <xsl:variable name="objectClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>           
           objectLocalHomes.put("<xsl:value-of select="@ID"/>", ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>"));
         </xsl:for-each> 
          return objectLocalHomes;
        }


         <!--
         /**
         * @ejb:interface-method
         */
-->
         private Hashtable returnObjectIdManagers()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
         Hashtable env=new Hashtable();
         env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
         env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
         env.put("java.naming.provider.url","localhost");
         InitialContext ctx=new InitialContext(env);
         
         Hashtable idManagers=new Hashtable();

         IdManagerLocalHome idManagerLocalHome =(IdManagerLocalHome) ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.IdManager"); 
         IdManagerLocal idManagerLocal;

         <xsl:for-each select="//objectClass">
           <xsl:variable name="objectClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           
           try
           { 
            idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>");
           }
           catch(FinderException e)
           {
            System.out.println(e.getMessage());
            System.out.println("maybe IdManager has not been created for this table, trying ....");
            idManagerLocalHome.create("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>");
            idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>");
            System.out.println("yes, got it");
           }
           System.out.println("IdManagerKey. <xsl:value-of select="@ID"/>");
           idManagers.put("<xsl:value-of select="@ID"/>",idManagerLocal);
         </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return idManagers;
        }
         
         <!--         /**
         * @ejb:interface-method
         */
-->
         private Hashtable returnRelationLocalHomes()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
         Hashtable env=new Hashtable();
         env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
         env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
         env.put("java.naming.provider.url","localhost");
         InitialContext ctx=new InitialContext(env);
         
         Hashtable relationLocalHomes=new Hashtable();
         
         <xsl:for-each select="//relationClass">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>           
           relationLocalHomes.put("<xsl:value-of select="@ID"/>", ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>"));
         </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return relationLocalHomes;
        }

         <!--
         /**
         * @ejb:interface-method
         */
              -->
         private Hashtable returnRelationIdManagers()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
         Hashtable env=new Hashtable();
         env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
         env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
         env.put("java.naming.provider.url","localhost");
         InitialContext ctx=new InitialContext(env);
         
         Hashtable idManagers=new Hashtable();

         IdManagerLocalHome idManagerLocalHome =(IdManagerLocalHome) ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.IdManager"); 
         IdManagerLocal idManagerLocal;
         
         <xsl:for-each select="//relationClass">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           
           try
           { 
            idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>");
           }
           catch(FinderException e)
           {
            System.out.println(e.getMessage());
            System.out.println("maybe IdManager has not been created for this table, trying ....");
            idManagerLocalHome.create("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>");
            idManagerLocal=idManagerLocalHome.findByPrimaryKey("<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>");
            System.out.println("yes, got it");
           }
           idManagers.put("<xsl:value-of select="@ID"/>",idManagerLocal);
         </xsl:for-each> 
         //Todo: Speichern und immer verwenden, solange der client aktiv ist
         return idManagers;
        }
         

         /**
         * @ejb:interface-method
         */
         public void importFromXml(Object vId,String fileName) throws
         javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException,
         java.io.IOException,
         org.xml.sax.SAXException
         {

          File outFile=new File(getTempDir(),fileName);
          fileName=outFile.getPath();

         /*
         System.out.println("VersionSession: importFromXml");
         System.out.println("VersionSession: get ObjectIdManagers");
         this.objectIdManagers=returnObjectIdManagers();
         System.out.println(" got it: "+objectIdManagers.toString());
         
         System.out.println("VersionSession: get RelationIdManagers");
         this.relationIdManagers=returnRelationIdManagers();
         System.out.println(" got it: "+relationIdManagers.toString());

         System.out.println("VersionSession: get ObjectLocalHomes");
         this.objectLocalHomes=returnObjectLocalHomes();
         System.out.println(" got it: "+objectLocalHomes.toString());
         
         System.out.println("VersionSession: get RelationLocalHomes");
         this.relationLocalHomes=returnRelationLocalHomes();
         System.out.println(" got it: "+relationLocalHomes.toString());

         */
         this.importIdMapping=new Hashtable();

         XmlImport xmlImport=new XmlImport(fileName,this);
         this.importXmlDestinationVersionLocal=getVersion(vId);
         xmlImport.start();
         }
         

         public void importObject(GisTransferObject gisTransferObject)
         {

          String tableName=gisTransferObject.getTableName();
          System.out.println("importObject: to tableName"+tableName);  
         //          Object oId=((IdManagerLocal) idManagerObjectobjectIdManagers.get(tableName)).useId();
          try
          {
           if(objectLocalHomes.containsKey(tableName))
           {
            // Objekte:
            Object idManagerObject=objectIdManagers.get(tableName);
            System.out.println("got IdManagerObject");
            Object oId=((IdManagerLocal) idManagerObject).useId();
            <xsl:for-each select="//objectClass">
              <xsl:variable name="objectClass">
                <xsl:call-template name="toUpperCase">
                  <xsl:with-param name="word" select="@ID"/>
                </xsl:call-template>
              </xsl:variable>           
              if("<xsl:value-of select="@ID"/>".equals(tableName))
              {
              System.out.println("importObject: create New Object");

              Object objectLocalHome = objectLocalHomes.get(tableName);

              System.out.println("importObject: got objectLocalHome");

              <xsl:value-of select="$objectClass"/>LocalHome specialLocalHome=(<xsl:value-of select="$objectClass"/>LocalHome)objectLocalHome;

              System.out.println("importObject: got scecialLocalHome");

              Object newObject= specialLocalHome.create((Integer)oId,importXmlDestinationVersionLocal);
              
              if(!importIdMapping.containsKey(tableName))
               importIdMapping.put(tableName,new Hashtable());
              ((Hashtable)importIdMapping.get(tableName)).put(gisTransferObject.getIdentifier(),oId);
              System.out.println(gisTransferObject.getIdentifier()+" maps to "+oId);
              System.out.println("importObject: created new Object");

              ElementLocal localElement=(ElementLocal) newObject;

              System.out.println("importObject: casted new Object");

              localElement.loadGisTransferObject(gisTransferObject);
              
              }
            </xsl:for-each>
           }                       
           else if(relationLocalHomes.containsKey(tableName))
           {
            // Relationen
            Object idManagerObject=relationIdManagers.get(tableName);
            System.out.println("got IdManagerObject");
            Object oId=((IdManagerLocal) idManagerObject).useId();
            <xsl:for-each select="//relationClass">
              <xsl:variable name="relationClass">
                <xsl:call-template name="toUpperCase">
                  <xsl:with-param name="word" select="@ID"/>
                </xsl:call-template>
              </xsl:variable>           
              if("<xsl:value-of select="@ID"/>".equals(tableName))
              {
               System.out.println("importObject: create New Realtion");
               Object relationLocalHome = relationLocalHomes.get(tableName);
               System.out.println("importObject: got relationLocalHome");
               <xsl:value-of select="$relationClass"/>LocalHome specialLocalHome=(<xsl:value-of select="$relationClass"/>LocalHome)relationLocalHome;
               System.out.println("importObject: got specialLocalHome");
               String srcTable=gisTransferObject.getRelationSrcTable();
               String destTable=gisTransferObject.getRelationDestTable();
               String srcId=gisTransferObject.getRelationSrcIdentifier();
               String destId=gisTransferObject.getRelationDestIdentifier();
               // find src-DB-Object
               if(importIdMapping.containsKey(srcTable) <![CDATA[&& ]]> importIdMapping.containsKey(destTable))
               {
                System.out.println("importObject: importMapping...");
                Object srcDbId = ((Hashtable)importIdMapping.get( srcTable)).get( srcId);
                Object destDbId= ((Hashtable)importIdMapping.get(destTable)).get(destId);
                Object srcObject=null;;
                Object destObject=null;               
                System.out.println("importObject: xmlIds: from "+srcId.toString()+" to "+destId.toString());
                System.out.println("importObject:  dbIds: from "+srcDbId.toString()+" to "+destDbId.toString());
                <xsl:for-each select="fromObjectClass">
                  <xsl:variable name="srcClass">
                    <xsl:call-template name="toUpperCase">
                      <xsl:with-param name="word" select="@ref"/>
                    </xsl:call-template>
                  </xsl:variable>
                  if("<xsl:value-of select="@ref"/>".equals(srcTable))
                  srcObject=((<xsl:value-of select="$srcClass"/>LocalHome)objectLocalHomes.get(srcTable)).findByPrimaryKey((Integer)srcDbId);
                </xsl:for-each>
                <xsl:for-each select="toObjectClass">
                  <xsl:variable name="destClass">
                    <xsl:call-template name="toUpperCase">
                      <xsl:with-param name="word" select="@ref"/>
                    </xsl:call-template>
                  </xsl:variable>
                  if("<xsl:value-of select="@ref"/>".equals(destTable))
                   destObject=((<xsl:value-of select="$destClass"/>LocalHome)objectLocalHomes.get(destTable)).findByPrimaryKey((Integer)destDbId);
                </xsl:for-each>
                if(srcObject!=null <![CDATA[&& ]]> destObject != null)
                {
                 Object newObject=specialLocalHome.create((Integer)oId,importXmlDestinationVersionLocal,srcObject,destObject);
                 System.out.println("importObject: created new Relation");
                 ElementLocal localElement=(ElementLocal) newObject;
                 System.out.println("importObject: casted new Relation");
                 localElement.loadGisTransferObject(gisTransferObject);
                }
                else
                {
                  System.out.println(":-( cannot create relation between "+srcTable+"/"+srcId.toString()+" or "+destTable+"/"+destId.toString());
                  System.out.println(":-(   check references in xml-input-file");
                } 
              }
               else
                System.out.println(":-( unknown "+srcTable+" or "+destTable);
              }
            </xsl:for-each>
           }                       
            else
             System.out.println(":-( "+tableName+" from  xml-file is unknown in this theme");
          } 
          catch(javax.ejb.CreateException e)
          {
           System.out.println("xmlImport:"+e.getMessage());
          }
            <xsl:if test="count(//relationClass) &gt; 0">
              catch(javax.ejb.FinderException e)
              {
              System.out.println("xmlImport:"+e.getMessage());
              }
            </xsl:if>
         }

         /**
         * @ejb:interface-method
         */
         public void run(String command)
         {
            System.out.println("EXECUTING: "+command);
            String dummy[]={""};	
            byte buffer[]=new byte[80];
            int c;
            try
	    {
             Process process=(Runtime.getRuntime()).exec(command);//,dummy,workingDir);
             InputStream stream=process.getInputStream();	
             while( (c=stream.read(buffer))!=-1)
             {
              String out=new String(buffer,0,c);
              System.out.print(out);
             }		
             process.waitFor();
	    }
            catch(Exception e)
	    {
             System.out.println(e.getMessage());
	    }

         }

            public static File getTempDir() throws java.io.IOException
            {
            // is there any better way ??
            File temp=File.createTempFile("egal","txt");
            File tempDir=temp.getParentFile();
            temp.delete();
            return tempDir;
    }




}





























         
         package <xsl:value-of select="$package"/>;
         
         import javax.ejb.CreateException;
         import javax.ejb.EntityBean;
         import javax.ejb.EntityContext;
         import javax.ejb.FinderException;

         import javax.naming.InitialContext;

         
            //         import de.tuhh.wb.javagis.model.IdManagerLocalHome;
            //         import de.tuhh.wb.javagis.model.IdManagerLocal;
         import de.tuhh.wb.javagis.model.ElementLocal;
         import de.tuhh.wb.javagis.model.ElementSession;
         import de.tuhh.wb.javagis.model.Tools;    
         
         import java.util.Hashtable;
         import java.util.ArrayList;
         import java.util.Collection;
         import java.util.Iterator;  

         <!--===============================================================================-->
         <!--                                                                               -->
         <!--                             EntityBean                                        -->
         <!--                                                                               -->
         <!--===============================================================================-->
         
         /**
         * @ejb:bean   name="<xsl:value-of select="$themeName"/>.Version"
         *             jndi-name="ejb/<xsl:value-of select="$themeName"/>.Version"
         *             local-jndi-name="<xsl:value-of select="$themeName"/>.Version"    
         *             view-type="local"
         *             type="CMP"
         *             cmp-version="2.x"
         *             schema="<xsl:value-of select="$themeName"/>_Version"
         *             primkey-field="id"
         * @ejb:interface local-extends="de.tuhh.wb.javagis.model.VersionLocal"
         * @ejb:pk generate="false" class="java.lang.Integer"
         * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.Version" view-type="local"
         * @ejb:finder signature="java.util.Collection findAll()" query="SELECT OBJECT(a) FROM <xsl:value-of select="$themeName"/>_Version AS a"
         *
         * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.IdManager"
         *              view-type="local"
         *              ref-name="ejb/<xsl:value-of select="$themeName"/>.IdManager"
         * @jboss:create-table create="true"
<!--         * @jboss:remove-table remove="true" -->
         *    
         */
    public abstract class VersionBean implements EntityBean
    {
    
    private transient EntityContext ctx;    

    // Relations...    
    <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
      <xsl:variable name="objectClass">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@ID"/>
        </xsl:call-template>
      </xsl:variable>
      /**
      * @ejb:interface-method
      * @ejb:relation name="Versioning-of-<xsl:value-of select="$objectClass"/>"
      *               role-name="Version-has-<xsl:value-of select="$objectClass"/>"
      */
      public abstract Collection get<xsl:value-of select="$objectClass"/>();
      
      /**
      * @ejb:interface-method
      */     
      public abstract void set<xsl:value-of select="$objectClass"/>(Collection col);      
    </xsl:for-each>

    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setId(Integer objectId);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract Integer getId();
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setVersionName(String name);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract String getVersionName();
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setVersionProject(String project);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract String getVersionProject();

    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setVersionState(String state);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract String getVersionState();
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setVersionDescription(String description);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract String getVersionDescription();        
         
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract void setVersionHistory(String history);
    
    /**
    * @ejb:interface-method
    * @ejb:persistent-field
    */
    public abstract String getVersionHistory();

    /**
    * @ejb:interface-method
    */
    public int getNumberOfObjects()
    {
    int number=0;
    <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
      <xsl:variable name="objectClass">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@ID"/>
        </xsl:call-template>
      </xsl:variable>
      number+=get<xsl:value-of select="$objectClass"/>().size();
      
    </xsl:for-each>

    return number;
    }
    
    /**
    * @ejb:create-method
    */
    public Integer ejbCreate(Integer id) throws CreateException
    {
      if(id == null)  throw new CreateException("Id is null :-(");
    
      this.setId(id);
      return null;
    }
         
    public void ejbPostCreate(Integer objectId)
    {   
    }
    
    // Methoden des Entity-Bean-Interface:
    public void ejbActivate()
    {}

    public void ejbPassivate()
    {}
    
    public void setEntityContext(EntityContext ctx)
    {
    this.ctx=ctx;
    }
    
    public void unsetEntityContext()
    {
    this.ctx=null;
    }
    
    public void ejbLoad()
    {}
    public void ejbStore()
    {}
    
    public void ejbRemove() throws javax.ejb.RemoveException
    {}
    

   } 
    
    
    

       </xsl:template>
       
       <xsl:template name="toUpperCase">
         <xsl:param name="word"/>
         <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
         <xsl:value-of select="substring($word,2)"/>
       </xsl:template>
       
     </xsl:stylesheet>
     