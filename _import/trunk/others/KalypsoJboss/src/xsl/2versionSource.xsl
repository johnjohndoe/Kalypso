<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text"/>
  <xsl:variable name="package" select="/theme/package"/>
  
  <xsl:template match="/theme">
    <xsl:variable name="themeKey" select="@ID"/>

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
         import javax.ejb.EJBLocalObject;
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

         import de.tuhh.wb.javagis.model.ElementLocal;
         import de.tuhh.wb.javagis.model.RelationLocal;
         import de.tuhh.wb.javagis.model.ObjectLocal;

         import de.tuhh.wb.javagis.xml.GisTransferObject;
         import de.tuhh.wb.javagis.xml.XmlImport;
         import de.tuhh.wb.javagis.xml.KalypsoXmlImportListener;
         import org.xml.sax.SAXException;

         import java.io.IOException;
         import java.io.Writer;
         import java.io.StringWriter;
         import java.io.PrintWriter;
         import java.io.FileWriter;
         import java.io.File;

         import java.io.File;
         import java.io.IOException;
         import java.util.Properties;
         import java.io.InputStream;
         import java.io.FileInputStream;
         import java.io.FileOutputStream;
         import de.tuhh.wb.javagis.model.BasePointTransfer;         

                 import ejb.event.EJBEvent;
                 import javax.jms.JMSException;
                 import javax.naming.NamingException;   
                 import ejb.event.EJBEventHelper;         
         
    import org.xml.sax.InputSource;
    import java.io.StringReader;
    import java.io.FileInputStream;



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

         public class VersionSessionBean implements SessionBean, KalypsoXmlImportListener 
         {
         private static int MAX_XML_SIZE=1024*1024; // 1MB

         private transient Hashtable env; //Environment
         private transient IdManagerLocalHome idManagerLocalHome;
         private transient VersionLocalHome myVersionLocalHome;
         private transient Hashtable objectLocalHomes;
         private transient Hashtable relationLocalHomes;
         private SessionContext ctx=null;

         private transient Object importXmlDestinationVID;

         private Object objectLocalHomes(String objectKey)
         {
          return objectLocalHomes.get(objectKey);
         }
         private Object relationLocalHomes(String relationKey)
         {
          return relationLocalHomes.get(relationKey);
         }
         private Object elementLocalHomes(String elementKey)
         {
          if(objectLocalHomes.containsKey(elementKey))
            return objectLocalHomes.get(elementKey);
          if(relationLocalHomes.containsKey(elementKey))
            return relationLocalHomes.get(elementKey);
          System.out.println("elementLocalHomes: could not finnd Entry for "+elementKey);
          return null;
         }


         private transient Hashtable importIdMapping;

         /**
         * @ejb:create-method
         */
         public void ejbCreate() throws CreateException
         {
          ejbActivate();
         }         

         public void ejbActivate() // prepare for activate
         {
         //          System.out.println("activate...");
          this.env=new Hashtable();
          env.put("java.naming.factory.initial","org.jnp.interfaces.NamingContextFactory");
          env.put("java.naming.factory.url.pkgs","org.jboss.naming:org.jnp.interfaces");
          env.put("java.naming.provider.url","localhost");

          try
          {
             InitialContext ctx=new InitialContext(env);
             this.myVersionLocalHome =(VersionLocalHome)
             ctx.lookup("java:comp/env/ejb/Versions");
             this.idManagerLocalHome =(IdManagerLocalHome) ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.IdManager"); 
             importXmlDestinationVID=null;

             setObjectLocalHomes();
             setRelationLocalHomes();
          }
          catch(Exception e)
          {
          e.printStackTrace();
          } 
         }

         public void ejbRemove() // free recources
         {    
          this.env=null;
          this.idManagerLocalHome=null;
          this.myVersionLocalHome=null;
          this.objectLocalHomes=null;
          this.relationLocalHomes=null;
          this.importXmlDestinationVID=null;
         //          System.out.println("remove VersionSession...");
         }
         
         public void ejbPassivate() // prepare for passivate
         {
          this.env=null;
          this.idManagerLocalHome=null;
          this.myVersionLocalHome=null;
          this.objectLocalHomes=null;
          this.relationLocalHomes=null;
          this.importXmlDestinationVID=null;
         //          System.out.println("passivate VersionSession...");
         }

         private void setObjectLocalHomes()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
          InitialContext ctx=new InitialContext(env);
          this.objectLocalHomes=new Hashtable();         

          <xsl:for-each select="//objectClass">
            <xsl:variable name="objectClass">
              <xsl:call-template name="toUpperCase">
                <xsl:with-param name="word" select="@ID"/>
              </xsl:call-template>
            </xsl:variable>           
            objectLocalHomes.put("<xsl:value-of select="@ID"/>", ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>"));
          </xsl:for-each> 
         }

         private void setRelationLocalHomes()
         throws javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException
         {
          InitialContext ctx=new InitialContext(env);
          this.relationLocalHomes=new Hashtable();
         <xsl:for-each select="//relationClass">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>           
           relationLocalHomes.put("<xsl:value-of select="@ID"/>", ctx.lookup("java:comp/env/ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$relationClass"/>"));
         </xsl:for-each> 
         }


         private IdManagerLocal getIdManagerLocal(String tableKey) throws CreateException, FinderException
         {
          IdManagerLocal idManagerLocal;
             try
             { 
               idManagerLocal=idManagerLocalHome.findByPrimaryKey(tableKey);//"<xsl:value-of select="$themeName"/>.Version");
             }
             catch(FinderException e)
             {
               System.out.println(e.getMessage());
               System.out.println("maybe IdManager has not been created for table \""+tableKey+"\", trying ....");
               idManagerLocalHome.create(tableKey);
               idManagerLocal=idManagerLocalHome.findByPrimaryKey(tableKey);
               System.out.println("yes, got it");
             }
          return idManagerLocal;
         }

         private VersionLocal getVersion(Object oId) throws FinderException
         {
          VersionLocal version=myVersionLocalHome.findByPrimaryKey((Integer)oId);
          return version;
         }


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
         public void renameVersion(Object vId,String project,String name,String state,String description,String history)
         throws javax.ejb.FinderException
         {
          VersionLocal version=getVersion(vId);
          version.setVersionProject(project);
          version.setVersionName(name);
          version.setVersionState(state);
          version.setVersionDescription(description);
          version.setVersionHistory(history);
          try
          {
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_RENAME,vId,-1,null);
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
         //           System.out.println("fired SimplePropertyEvent");
          }
          catch(Exception e)
          {
           e.printStackTrace();
          }          
         }
         


         /**
         * @ejb:interface-method
         */
         public Vector getPrimaryKeyList() throws javax.ejb.FinderException
         {
           Collection col=myVersionLocalHome.findAll();
           Vector primaryKeyList=new Vector();
         //           System.out.println("VersionSession:  got myObjects ("+col.size()+" elements) ... ");
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
         return createVersion( project, name, state, description, history,true);
        
         }

         public Object createVersion(String project,String name,String state,String description,String history,boolean withEvent) throws CreateException
         {
          Integer newId=null;
          try
         {
          newId=getIdManagerLocal("<xsl:value-of select="$themeName"/>.Version").useId();
         }
         catch(FinderException e)
         {
          throw new CreateException(e.getMessage());
         }
          VersionLocal newVersion = myVersionLocalHome.create(newId,withEvent);
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





         private void exportToXml(Writer out,Collection col) throws IOException
         {
          Iterator it=col.iterator();
          while(it.hasNext())
          {
           GisTransferObject gto=((ElementLocal)it.next()).toGisTransferObject();
           out.write(gto.toXmlString());
          } 
         }


         // OK
         <!-- -->
         /**
         * @ejb:interface-method
         */
         public void exportToXml(Object vId,File exportFile) throws
         FinderException,
         javax.naming.NamingException,
         java.io.IOException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          exportToXml(vId,new PrintWriter(new FileWriter(exportFile)));
         }

         /**
         * @ejb:interface-method
         */
         public String exportToXml(Object vId) throws
         FinderException,
         javax.naming.NamingException,
         java.io.IOException,
         java.rmi.RemoteException,
         javax.ejb.CreateException
         {
          StringWriter stringWriter=new StringWriter(MAX_XML_SIZE);
          exportToXml(vId,stringWriter);
          return stringWriter.toString();
         }

         public void exportToXml(Object vId,Writer out) throws
         FinderException,
         java.io.IOException
         {
          Tools.genXmlTag(out,"?xml version=\"1.0\" encoding=\"ISO-8859-1\"?");
         //         Tools.genXmlTag(out,"?xml version=\"1.0\" encoding=\"UTF-8\"?");
         Tools.genXmlTag(out,"theme");

         VersionLocal version=myVersionLocalHome.findByPrimaryKey((Integer)vId);
         Collection col=null;         
         <xsl:for-each select="/theme/objectClass">
           <xsl:variable name="name">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           col=version.get<xsl:value-of select="$name"/>();
           exportToXml(out,col);
         </xsl:for-each>
         <xsl:for-each select="/theme/relationClass">
           <xsl:variable name="name">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           col=version.get<xsl:value-of select="$name"/>();
           exportToXml(out,col);
         </xsl:for-each>

           Tools.genXmlTag(out,"/theme");
           out.close();  
         }

         /**
         * @ejb:interface-method
         */
         public void copyVersion(Object vId,String project,String name,String state,String description,String hostory)
         throws NamingException,
         CreateException,
         FinderException,
         SAXException
         {
         
          Object targetVersionId=createVersion( project, name, state, description,"copy",false );

          this.importIdMapping=new Hashtable();
          this.importXmlDestinationVID=targetVersionId;

         VersionLocal version=myVersionLocalHome.findByPrimaryKey((Integer)vId);
         Collection col=null;         
         Iterator it=null;
         <xsl:for-each select="/theme/objectClass">
           <xsl:variable name="name">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           col=version.get<xsl:value-of select="$name"/>();
           it=col.iterator();
           while(it.hasNext())
           {
            GisTransferObject gto=((ElementLocal)it.next()).toGisTransferObject();
            importObject(gto);
           } 
         
         </xsl:for-each>
         <xsl:for-each select="/theme/relationClass">
           <xsl:variable name="name">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="@ID"/>
             </xsl:call-template>
           </xsl:variable>
           col=version.get<xsl:value-of select="$name"/>();

           it=col.iterator();
           while(it.hasNext())
           {
            GisTransferObject gto=((ElementLocal)it.next()).toGisTransferObject();
            importObject(gto);
           } 

         </xsl:for-each>

         //EVENT:
         try
         {
         //          System.out.println("fire VersionEvent");
          EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_CREATE,targetVersionId,-1,null);
          EJBEventHelper eventHelper=new EJBEventHelper();
          eventHelper.fireEvent(event);
         //          System.out.println("fired VersionEvent");
         }
         catch(NamingException e)
         {
          e.printStackTrace();
         }
         catch(JMSException e)
         {
          e.printStackTrace();
         }


         }
         

         //		StringReader stringReader=new StringReader(xmlString);
         //		InputSource inputSource=new InputSource(stringReader);
         //		reader.parse(inputSource);
         //		xmlString=null;


         /**
         * @ejb:interface-method
         */
         public void importFromXml(Object vId,String xmlString) throws
         javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException,
         java.io.IOException,
         org.xml.sax.SAXException
         {
          StringReader stringReader=new StringReader(xmlString);
          InputSource inputSource=new InputSource(stringReader);
          importFromXml(vId,inputSource);
          xmlString=null;
         }

         public void importFromXml(Object vId,InputSource inputSource) throws
         javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException,
         java.io.IOException,
         org.xml.sax.SAXException
         {
          this.importIdMapping=new Hashtable();
         
          XmlImport xmlImport=new XmlImport(inputSource,this);
          this.importXmlDestinationVID=vId;

          xmlImport.start();

         //EVENT:
         try
         {
         //         System.out.println("fire VersionEvent");
         EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_CLOSEVIEWS,vId,-1,null);
         EJBEventHelper eventHelper=new EJBEventHelper();
         eventHelper.fireEvent(event);
         //         System.out.println("fired VersionEvent");
         }
         catch(NamingException e)
         {
         e.printStackTrace();
         }
         catch(JMSException e)
         {
         e.printStackTrace();
         }
         }
         
         /**
         * @ejb:interface-method
         */
         public void importFromXml(Object vId,File importFile) throws
         javax.naming.NamingException,
         javax.ejb.CreateException,
         javax.ejb.FinderException,
         java.io.IOException,
         org.xml.sax.SAXException
         {
          FileInputStream inputStream=new FileInputStream(importFile);
          InputSource inputSource=new InputSource(inputStream);
          importFromXml(vId,inputSource);
          inputStream.close();
         }

         /*
          this.importIdMapping=new Hashtable();
          XmlImport xmlImport=new XmlImport(importFile,this);
          this.importXmlDestinationVID=vId;

          xmlImport.start();

         //EVENT:
         try
         {
         //         System.out.println("fire VersionEvent");
         EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_CLOSEVIEWS,vId,-1,null);
         EJBEventHelper eventHelper=new EJBEventHelper();
         eventHelper.fireEvent(event);
         //         System.out.println("fired VersionEvent");
         }
         catch(NamingException e)
         {
         e.printStackTrace();
         }
         catch(JMSException e)
         {
         e.printStackTrace();
         }
         }
         */

         public void importObject(GisTransferObject gto)
         {
          String tableName=gto.getTableName();
         //          System.out.println("importObject: to tableName"+tableName);  
          try
          {
            if(objectLocalHomes.containsKey(tableName))
            {
              int objectTable=getObjectTable(tableName);
              Object oId=objectCreate(objectTable,importXmlDestinationVID,false);
              if(!importIdMapping.containsKey(tableName))
                importIdMapping.put(tableName,new Hashtable());
              ((Hashtable)importIdMapping.get(tableName)).put(gto.getIdentifier(),oId);
         //              System.out.println(gto.getIdentifier()+" maps to "+oId);
         //              System.out.println("importObject: created new Object");
              ObjectLocal element=getObjectLocal(objectTable,oId);                                  
         //              System.out.println("importObject: casted new Object");
              element.loadGisTransferObject(gto);
            }
            else if(relationLocalHomes.containsKey(tableName))
            {
              // Relationen
              int relationTable=getRelationTable(tableName);
              String srcKey=gto.getRelationSrcTable();
              String destKey=gto.getRelationDestTable();
              String srcId=gto.getRelationSrcIdentifier();
              String destId=gto.getRelationDestIdentifier();
              // find src-DB-Object
         if(importIdMapping.containsKey(srcKey) <![CDATA[&& ]]> importIdMapping.containsKey(destKey))
              {
         //                System.out.println("importObject: importMapping...");
                Object srcDbId = ((Hashtable)importIdMapping.get( srcKey)).get( srcId);
                Object destDbId= ((Hashtable)importIdMapping.get(destKey)).get(destId);
         //                System.out.println("importObject: xmlIds: from "+srcId.toString()+" to "+destId.toString());
         //                System.out.println("importObject:  dbIds: from "+srcDbId.toString()+" to "+destDbId.toString());
                Object rId=createRelation(relationTable,importXmlDestinationVID,srcKey,srcDbId,destKey,destDbId,false);
         //                System.out.println("importObject: create New Realtion");
                RelationLocal element=getRelationLocal(relationTable,rId);
                element.loadGisTransferObject(gto);
              }
              else
              {
         //                System.out.println(":-( cannot create relation between "+srcKey+"/"+srcId.toString()+" or "+destKey+"/"+destId.toString());
         //                System.out.println(":-(   check references in xml-input-file");
              } 
            }
            else
             System.out.println(":-( "+tableName+" from  xml-file is unknown in this theme");
          }
          catch(javax.ejb.CreateException e)
          {
         //            System.out.println("xmlImport: :-("+e.getMessage());
          }
         <!--         <xsl:if test="count(//relationClass) &gt; 0"> -->
          catch(javax.ejb.FinderException e)
          {
         //            System.out.println("xmlImport: :-("+e.getMessage());
          }

         <!--         </xsl:if> -->
         }

         /**
         * @ejb:interface-method
         */
         public void run(String command)
         {
         //            System.out.println("EXECUTING: "+command);
         /*            String dummy[]={""};	
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
         */
         }

            private static File getTempDir() throws java.io.IOException
            {
            // is there any better way ??
            File temp=File.createTempFile("egal","txt");
            File tempDir=temp.getParentFile();
            temp.delete();
            return tempDir;
            }


            // emulated ElementSession starts here:
            

            // constants:
            private final static String[][] simplePropertyKeys={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="simpleProperty">"<xsl:value-of select="@key"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetSimplePropertyKeys(int elementTable)
            {
            return simplePropertyKeys[elementTable];
            }
            
            private final static String[][] simplePropertyNames={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="simpleProperty">"<xsl:value-of select="name"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetSimplePropertyNames(int elementTable)
            {
            return simplePropertyNames[elementTable];
            }
            
            private final static String[][] simplePropertyDescriptions={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="simpleProperty">"<xsl:value-of select="description"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetSimplePropertyDescriptions(int elementTable)
            {
            return simplePropertyDescriptions[elementTable];
            }

            private final static String[][] simplePropertyUnits={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">

              {<xsl:for-each select="simpleProperty">"<xsl:copy-of select="unit"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>

            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetSimplePropertyUnits(int elementTable)
            {
             return simplePropertyUnits[elementTable];
            }
      
            private final static Class[][] simplePropertyClasses={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="simpleProperty"><xsl:value-of select="type"/>.class<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public Class[] elementGetSimplePropertyClasses(int elementTable)
            {
            return simplePropertyClasses[elementTable];
            }
      
            private final static String[][] simplePropertyFormats={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="simpleProperty"><xsl:choose><xsl:when test="format">"<xsl:value-of select="format"/>"</xsl:when><xsl:otherwise>"d.M.y H:m:s"</xsl:otherwise></xsl:choose><xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetSimplePropertyFormats(int elementTable)
            {
            return simplePropertyFormats[elementTable];
            }
                    
            private final String[][] vectorSetKeys={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="vectorSet">"<xsl:value-of select="@key"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetVectorSetKeys(int elementTable)
            {
            return vectorSetKeys[elementTable];
            }
            
            private final static String[][] vectorSetNames={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="vectorSet">"<xsl:value-of select="name"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetVectorSetNames(int elementTable)
            {
            return vectorSetNames[elementTable];
            }
        
            private final static String[][] vectorSetDescriptions={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
              {<xsl:for-each select="vectorSet">"<xsl:value-of select="description"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] elementGetVectorSetDescriptions(int elementTable)
            {
            return vectorSetDescriptions[elementTable];
            }


           /**
           * @ejb:interface-method
           */
           public void elementSetVectorSet(int elementTable,Object eId,int pos,Object value) throws FinderException
           {
            ElementLocal element=getElementLocal(elementTable,eId);
            element.setVectorSet(pos,value);
           }

           /**
           * @ejb:interface-method
           */
           public Object elementGetVectorSet(int elementTable,Object eId,int pos) throws FinderException
           {
            ElementLocal element=getElementLocal(elementTable,eId);
            return element.getVectorSet(pos);
           }

           /**
           * @ejb:interface-method
           */
           public void elementSetVectorSets(int elementTable,Object eId,Vector vectorSets) throws FinderException
           {            
            ElementLocal element=getElementLocal(elementTable,eId);
            element.setVectorSets(vectorSets);
           }

            /**
            * @ejb:interface-method
            */
            public Vector elementGetVectorSets(int elementTable,Object eId) throws FinderException
            {
             ElementLocal element=getElementLocal(elementTable,eId);
             return element.getVectorSets();
            }


            // ElementConstants:
            private final static String[] elementKeys={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">"<xsl:value-of select="@ID"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] getElementKeys()
            {
            return elementKeys;
            }

            private final static String[] objectKeys={
            <xsl:for-each select="/theme/objectClass">"<xsl:value-of select="@ID"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] getObjectKeys()
            {
            return objectKeys;
            }

            private final static String[] relationKeys={
            <xsl:for-each select="/theme/relationClass">"<xsl:value-of select="@ID"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] getRelationKeys()
            {
            return relationKeys;
            }
            
            private final static String[] elementNames={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">"<xsl:value-of select="name"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] getElementNames()
            {
            return elementNames;
            }
            
            private final static String[] elementSymbolNames={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">"<xsl:value-of select="symbol"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] getElementSymbolNames()
            {
            return elementSymbolNames;
            }

            private final static String[] elementDescriptions={
            <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">"<xsl:value-of select="description"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
            /**
            * @ejb:interface-method
            */         
            public String[] getElementDescriptions()
            {
            return elementDescriptions;
            }
            
            /**
            * @ejb:interface-method
            */         
            public Hashtable elementGetBasePoints(int pos,Vector oIds) throws FinderException
            {
             Hashtable result=new Hashtable();
             for(int i=0;i<![CDATA[<]]>oIds.size();i++)
              {
                result.put(oIds.elementAt(i),getElementLocal(pos,oIds.elementAt(i)).getBasePoint());
              }
             return result;
            }

            /**
            * @ejb:interface-method
            */         
            public BasePointTransfer elementGetBasePoint(int pos,Object oId) throws FinderException
            {
             return getElementLocal(pos,oId).getBasePoint();
            }
            
            /**
            * @ejb:interface-method
            */         
            public void elementSetBasePoint(int pos,Object oId, BasePointTransfer point) throws FinderException
            {
             getElementLocal(pos,oId).setBasePoint(point);
            }            
            
            /**
            * @ejb:interface-method
            */         
            public void elementSetSimpleProperty(int table,Object oId, int pos,Object value) throws FinderException
            {
              ElementLocal element=getElementLocal(table,oId);
              element.setSimpleProperty(pos,value);
              //EVENT:
              try
              {
            //                System.out.println("fire SimplePropertyEvent");
                EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.SIMPLEPROPERTY_CHANGE,element.getVersion().getPrimaryKey(),table,element.getId());
                EJBEventHelper eventHelper=new EJBEventHelper();          
                eventHelper.fireEvent(event);
            //                System.out.println("fired SimplePropertyEvent");
              }
                catch(NamingException e)
              {
                e.printStackTrace();
              }
              catch(JMSException e)
              {
                e.printStackTrace();
              }
            }  
            
            /**
            * @ejb:interface-method
            */         
            public Object elementGetSimpleProperty(int elementTable,Object oId, int pos) throws FinderException
            {
             return getElementLocal(elementTable,oId).getSimpleProperty(pos);
            }

            /**
            * @ejb:interface-method
            */         
            public Vector elementGetSimplePropertyRow(int elementTable,Object oId) throws FinderException
            {
             return getElementLocal(elementTable,oId).getSimplePropertyRow();
            }


            /**
            * @ejb:interface-method
            */         
            public Hashtable elementGetSimplePropertyRows(int elementTable,Vector primKeys) throws FinderException

            {
             Hashtable result=new Hashtable();
             for(int n=0;n <![CDATA[ < ]]> primKeys.size();n++)
             {
              Object primKey=primKeys.elementAt(n);
              result.put(primKey,elementGetSimplePropertyRow(elementTable,primKey));
             }
             return result;
            }




            /**
            * @ejb:interface-method
            */
            public Vector getPrimaryKeyList(int elementTable,Object vId) throws FinderException  
            {
             VersionLocal version=myVersionLocalHome.findByPrimaryKey((Integer)vId);
             Collection col=null;
             switch(elementTable)
             {
              <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                case <xsl:value-of select="position()-1"/>:                
                 col=version.get<xsl:value-of select="$name"/>();
                break;
              </xsl:for-each>
                default:
                 break;
              }
              Vector primaryKeyList=new Vector();
              //              System.out.println("  got myObjects ("+col.size()+" elements) ... ");
              if(col!=null)
              {
               Iterator it=col.iterator();
               while(it.hasNext())
               {
                primaryKeyList.add(((EJBLocalObject) it.next()).getPrimaryKey());
               } 
              }
              return primaryKeyList;
           }





              private final static boolean[] elementIsRelation={
              <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]"><xsl:choose><xsl:when test="local-name()='relationClass'">true</xsl:when><xsl:otherwise>false</xsl:otherwise></xsl:choose><xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
              /**
              * @ejb:interface-method
              */
              public boolean[] elementIsRelation()
              {
               return elementIsRelation;
              }

              //Relations:
              private final static String[] relationForwardLabels={
              <xsl:for-each select="/theme/relationClass">"<xsl:value-of select="forwardLabel"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
              /**
              * @ejb:interface-method
              */
              public String[] relationGetForwardLabels()
              {
               return relationForwardLabels;
              }

              private final static String[] relationBackwardLabels={
              <xsl:for-each select="/theme/relationClass">"<xsl:value-of select="backwardLabel"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
              /**
              * @ejb:interface-method
              */
              public String[] relationGetBackwardLabels()
              {
               return relationBackwardLabels;
              }

            private final static String[][] relationSourceKeys={
            <xsl:for-each select="/theme/relationClass">
              {<xsl:for-each select="fromObjectClass">"<xsl:value-of select="@ref"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] relationGetSourceKeys(int relationTable)
            {
            return relationSourceKeys[relationTable];
            }

            private final static String[][] relationDestinationKeys={
            <xsl:for-each select="/theme/relationClass">
              {<xsl:for-each select="toObjectClass">"<xsl:value-of select="@ref"/>"<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>};
            /**
            * @ejb:interface-method
            */
            public String[] relationGetDestinationKeys(int relationTable)
            {
            return relationDestinationKeys[relationTable];
            }

          /**
          * @ejb:interface-method
          */
          public Vector getRelationVector(int relationTable,Object rId) throws javax.ejb.FinderException
          {
            RelationLocal relation=getRelationLocal(relationTable,rId);

            Vector result=new Vector();
            Object srcId =null;
            String srcKey=null;
            Object destId =null;
            String destKey=null;

            srcId   =relation.getSrcId();
            srcKey  =relation.getSrcKey();
            destId  =relation.getDestId();
            destKey =relation.getDestKey();
            
            result.add(srcId);
            result.add(srcKey);
            result.add(destId);
            result.add(destKey);
            //            System.out.println(name()+"Session: RelationsVector is:");
            //            System.out.println(name()+"Session: srcID"+srcId.toString()+"  "+srcKey);
            //            System.out.println(name()+"Session: destID"+destId.toString()+"   "+destKey);
            return result;
          }





















            // Objects
            private final static String[][] objectForwardRelations=
            {
            <xsl:for-each select="/theme/objectClass">{
              <xsl:variable name="objectKey" select="@ID"/> 
              <xsl:for-each select="//fromObjectClass[@ref=$objectKey]">
                <xsl:variable name="relationClass"> 
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="../@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                "<xsl:value-of select="$relationClass"/>"
                <xsl:if test="position() != last()">,</xsl:if>
              </xsl:for-each>}
              <xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>
            };
            /**
            * @ejb:interface-method
            */
            public String[] objectForwardRelations(int objectTable)
            {
             return objectForwardRelations[objectTable];
            }
                         
            private final static String[][] objectBackwardRelations=
            {
            <xsl:for-each select="/theme/objectClass">{
              <xsl:variable name="objectKey" select="@ID"/>             
              <xsl:for-each select="//toObjectClass[@ref=$objectKey]">
                <xsl:variable name="relationClass"> 
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="../@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                "<xsl:value-of select="$relationClass"/>"<xsl:if test="position() != last()">,</xsl:if>
              </xsl:for-each>}<xsl:if test="position() != last()">,</xsl:if>
            </xsl:for-each>
            };
            /**
            * @ejb:interface-method
            */
            public String[] objectBackwardRelations(int objectTable)
            {
             return objectBackwardRelations[objectTable];
            }

           /**
           * @ejb:interface-method
           */
           public Vector objectGetForwardRelations(int objectTable,Object oId) throws FinderException
           {
            return getObjectLocal(objectTable,oId).returnForwardRelations();
           }

           /**
           * @ejb:interface-method
           */
           public Vector objectGetBackwardRelations(int objectTable,Object oId) throws FinderException
           {
            return getObjectLocal(objectTable,oId).returnBackwardRelations();
           }

            private ElementLocal getElementLocal(int elementTable,Object eId)  throws FinderException
            {
             String tableKey=elementKeys[elementTable];
             ElementLocal element=null;
              switch(elementTable)
              {
              <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                case <xsl:value-of select="position()-1"/>:
                element=(ElementLocal)((<xsl:value-of select="$name"/>LocalHome)elementLocalHomes(tableKey)).findByPrimaryKey((Integer)eId);
                break;
              </xsl:for-each>
              default:
              break;
              }
              return element;
            }

            private ObjectLocal getObjectLocal(String objectKey,Object oId)  throws FinderException
            {
              <xsl:for-each select="/theme/objectClass">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                if("<xsl:value-of select="@ID"/>".equals(objectKey))
                return (ObjectLocal)((<xsl:value-of select="$name"/>LocalHome)elementLocalHomes(objectKey)).findByPrimaryKey((Integer)oId);
              </xsl:for-each>
              return null;
            }

            private ObjectLocal getObjectLocal(int objectTable,Object oId) throws FinderException
            {
             String tableKey=elementKeys[objectTable];
             ObjectLocal objectLocal=null;
              switch(objectTable)
              {
              <xsl:for-each select="/theme/objectClass">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                case <xsl:value-of select="position()-1"/>:
                objectLocal=(ObjectLocal)((<xsl:value-of select="$name"/>LocalHome)elementLocalHomes(tableKey)).findByPrimaryKey((Integer)oId);
                break;
              </xsl:for-each>
              default:
              break;
              }
              return objectLocal;
            }

            private RelationLocal getRelationLocal(int relationTable,Object rId) throws FinderException
            {
             String tableKey=relationKeys[relationTable];
             RelationLocal relation=null;
              switch(relationTable)
              {
              <xsl:for-each select="/theme/relationClass">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                case <xsl:value-of select="position()-1"/>:
                relation=(RelationLocal)((<xsl:value-of select="$name"/>LocalHome)relationLocalHomes(tableKey)).findByPrimaryKey((Integer)rId);
                break;
              </xsl:for-each>
              default:
              break;
              }
              return relation;
            }

           /**
           * @ejb:interface-method
           */
           public Object objectCreate(int objectTable,Object vId) throws CreateException
              {
              return objectCreate(objectTable,vId,true);
              }

              private Object objectCreate(int objectTable,Object vId,boolean withEvent) throws CreateException
           {
             String objectKey=objectKeys[objectTable];
             Integer newId=null;
             VersionLocal version=null;
             try
              {
               newId=useId(objectKey);
               version=(VersionLocal)myVersionLocalHome.findByPrimaryKey((Integer)vId);
              }
              catch(FinderException e)
              {
               throw new CreateException(e.getMessage());
              }
             ElementLocal newObject=null;
             switch(objectTable)
             {
              <xsl:for-each select="/theme/objectClass">
                <xsl:variable name="name">
                  <xsl:call-template name="toUpperCase">
                    <xsl:with-param name="word" select="@ID"/>
                  </xsl:call-template>
                </xsl:variable>
                case <xsl:value-of select="position()-1"/>:
                  newObject=(ElementLocal)((<xsl:value-of select="$name"/>LocalHome)objectLocalHomes(objectKey)).create(newId,version,withEvent);
                break;
              </xsl:for-each>
              default:
              break;
              }
            return newObject.getPrimaryKey();
           }


              /**
              * @ejb:interface-method
              */
              public Object createRelation(int relationTable,Object vId,String srcKey,Object srcId,String destKey,Object destId) throws CreateException,FinderException
              {
              return createRelation(relationTable, vId, srcKey, srcId, destKey, destId,true);
              }

              private Object createRelation(int relationTable,Object vId,String srcKey,Object srcId,String destKey,Object destId,boolean withEvent) throws CreateException,FinderException
              {
               String relationKey=relationKeys[relationTable];
               VersionLocal version=null;
               Integer newId=null;
               try
               {
                version=(VersionLocal)myVersionLocalHome.findByPrimaryKey((Integer)vId);
                newId=useId(relationKey);
               }
               catch(FinderException e)
               {
                throw new CreateException(e.getMessage());
               }

               RelationLocal newRelation=null;
               if(!isAllowedRelation(relationTable,srcKey,destKey))
                 throw new CreateException("relation between this objects is not allowed");
               Object src=getObjectLocal(srcKey,srcId);
               Object dest=getObjectLocal(destKey,destId);
               if(src==null  <![CDATA[ || ]]> dest ==null)
                 throw new CreateException("couldn not found objects to relate");
               switch(relationTable)
               {
                 <xsl:for-each select="/theme/relationClass">
                   <xsl:variable name="name">
                     <xsl:call-template name="toUpperCase">
                       <xsl:with-param name="word" select="@ID"/>
                     </xsl:call-template>
                   </xsl:variable>
                   case <xsl:value-of select="position()-1"/>:
                   newRelation=(RelationLocal)((<xsl:value-of select="$name"/>LocalHome)relationLocalHomes(relationKey)).create(newId,version,src,dest,withEvent);
                   break;
                 </xsl:for-each>
                 default:
                 break;
               }
               if(newRelation!=null)
                 return newRelation.getPrimaryKey();
               throw new CreateException("couldn not create relation");
            }

                 /**
                 * @ejb:interface-method
                 */
                 public void removeElement(int elementTable,Object eId) throws RemoveException, FinderException
                 {
                  getElementLocal(elementTable,eId).remove();
                 }

            private boolean isAllowedRelation(int relationTable,String srcKey,String destKey)
            {
                 boolean allowed=false;
                 String[] srcKeys=relationGetSourceKeys(relationTable);
                 String[] destKeys=relationGetDestinationKeys(relationTable);
                 for(int i=0;i <![CDATA[ < ]]>srcKeys.length;i++)
                  if(srcKey.equals(srcKeys[i]))
                   allowed=true;
                 if(allowed==false)
                  return false;
                 allowed=false;
                 for(int i=0;i <![CDATA[ < ]]>destKeys.length;i++)
                  if(destKey.equals(destKeys[i]))
                   allowed=true;
                 return allowed;
              }

                 private Integer useId(int elementTable) throws FinderException,CreateException
                 {
                  return useId(elementKeys[elementTable]);
                 }

                 private Integer useId(String objectKey) throws FinderException,CreateException
                 {
                  String idManagerKey="<xsl:value-of select="$themeName"/>."+objectKey.substring(0,1).toUpperCase()+objectKey.substring(1);
                  IdManagerLocal idManagerLocal;
                  try
                  {
                   idManagerLocal=idManagerLocalHome.findByPrimaryKey(idManagerKey);
                  }
                  catch(FinderException e)
                  {
                   System.out.println(e.getMessage());
                   System.out.println("maybe IdManager has not been created for table \""+idManagerKey+"\", trying ....");
                   idManagerLocalHome.create(idManagerKey);
                   idManagerLocal=idManagerLocalHome.findByPrimaryKey(idManagerKey);
                   System.out.println("yes, got it");
                  }
                  return idManagerLocal.useId();
                 }

                 private int getElementTable(String elementKey) throws FinderException
                 {
                  for(int i=0;i<![CDATA[<]]>elementKeys.length;i++)
                   if(elementKey.equals(elementKeys[i]))
                    return i;
                  throw new FinderException(" Key \""+elementKey+"\" is not an elementKey :-(");
                 }

                 private int getObjectTable(String objectKey) throws FinderException
                 {
                  for(int i=0;i<![CDATA[<]]>objectKeys.length;i++)
                   if(objectKey.equals(objectKeys[i]))
                    return i;
                 throw new FinderException(" Key \""+objectKey+"\" is not an oibjectKey :-(");
                 }

                 private int getRelationTable(String relationKey) throws FinderException
                 {
                  for(int i=0;i<![CDATA[<]]>relationKeys.length;i++)
                   if(relationKey.equals(relationKeys[i]))
                    return i;
                  throw new FinderException(" Key \""+relationKey+"\" is not a relationKey :-(");
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

         import de.tuhh.wb.javagis.model.Tools;    

                 import ejb.event.EJBEvent;
                 import javax.jms.JMSException;
                 import javax.naming.NamingException;   
                 import ejb.event.EJBEventHelper;         
                 
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
         * @ejb:resource-ref res-name="ConnectionFactory"
         *                   res-type="javax.jms.TopicConnectionFactory"
         *                   res-auth="Container"
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
    public Integer ejbCreate(Integer vId,boolean withEvent) throws CreateException
    {
      if(vId == null)  throw new CreateException("Id is null :-(");
    
      this.setId(vId);
      return null;
    }
         
    public void ejbPostCreate(Integer vId,boolean withEvent)
    {   
     //EVENT:
     if(withEvent)
     {
      try
      {
    //       System.out.println("fire VersionEvent");
       EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_CREATE,vId,-1,null);
       EJBEventHelper eventHelper=new EJBEventHelper();
       eventHelper.fireEvent(event);
    //       System.out.println("fired VersionEvent");
      }
      catch(NamingException e)
      {
       e.printStackTrace();
      }
      catch(JMSException e)
      {
       e.printStackTrace();
      }
     }
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
    {
      try
      {
    //       System.out.println("fire VersionremoveEvent");
       EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VERSION_REMOVE,getId(),-1,null);
       EJBEventHelper eventHelper=new EJBEventHelper();
       eventHelper.fireEvent(event);
    //       System.out.println("fired VersionremoveEvent");
      }
      catch(NamingException e)
      {
       e.printStackTrace();
      }
      catch(JMSException e)
      {
       e.printStackTrace();
      }
    }
    




   } 
    
    
    

       </xsl:template>

       <xsl:template mode="unit" match="*">
         <xsl:copy-of select="."/>
       </xsl:template>
       
       <xsl:template name="toUpperCase">
         <xsl:param name="word"/>
         <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
         <xsl:value-of select="substring($word,2)"/>
       </xsl:template>
       
     </xsl:stylesheet>
     