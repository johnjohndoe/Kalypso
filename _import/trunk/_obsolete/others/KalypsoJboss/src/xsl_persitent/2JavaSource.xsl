<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text"/>
  <xsl:variable name="package" select="/theme/package"/>
  <xsl:variable name="themeName">
    <xsl:call-template name="toUpperCase">
      <xsl:with-param name="word" select="/theme/@ID"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="blank"> </xsl:variable>
  <xsl:template match="/">
    <xsl:apply-templates select="theme"/>
  </xsl:template>
  <xsl:template match="theme">
    <xsl:apply-templates mode="bean" select="child::*[self::objectClass or self::relationClass]"/>
    <xsl:apply-templates mode="session-bean" select="child::*[self::objectClass or self::relationClass]"/>
  </xsl:template>
  <xsl:template mode="session-bean" match="theme//child::*[self::objectClass or self::relationClass]"><xsl:variable name="objectClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable><xsl:variable name="relationClass"><xsl:value-of select="$objectClass"/></xsl:variable><!--===============================================================================--><!--                                                                               --><!--                             SessionBean                                       --><!--                                                                               --><!--===============================================================================-->

         package <xsl:value-of select="$package"/>;
         <xsl:if test="local-name()='objectClass'">
           // This SessionObject represents a Object
         </xsl:if><xsl:if test="local-name()='relationClass'">
           // This SessionObject represents a Relation
         </xsl:if>
         
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
         import java.util.Collection;
         import java.util.Iterator;
         import java.io.PrintWriter;         
         import de.tuhh.wb.javagis.model.Tools;    
         import java.io.FileWriter;
         import java.io.File;

         import de.tuhh.wb.javagis.property.ChartProperty;
         import de.tuhh.wb.javagis.property.VectorSet;
         //         import de.tuhh.wb.javagis.model.IdManagerLocal;
         //         import de.tuhh.wb.javagis.model.IdManagerLocalHome;
         import de.tuhh.wb.javagis.model.ElementLocal;
         import de.tuhh.wb.javagis.xml.GisTransferObject;

         import <xsl:value-of select="$package"/>.vectorsets.*;         

         //         import de.tuhh.wb.javagis.model.VersionLocal;

         /**
         * 
         * @ejb:bean   name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>Session&quot;
         *             jndi-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>Session&quot;
         *             local-jndi-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>Session&quot;
         *             view-type=&quot;remote&quot;
         *             type=&quot;Stateful&quot;
         *             cmp-version=&quot;2.x&quot;<!--
         --><xsl:if test="local-name()='objectClass'">
         * @ejb:interface extends=&quot;de.tuhh.wb.javagis.model.ObjectSession&quot;<!--
         --></xsl:if><xsl:if test="local-name()='relationClass'">
           * @ejb:interface extends=&quot;de.tuhh.wb.javagis.model.RelationSession&quot;<!--           
           --></xsl:if>
         * @ejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.Version&quot;
         *              view-type=&quot;remote&quot;
         *              ref-name=&quot;ejb/Versions&quot;
         * @ejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.IdManager&quot;
         *              view-type=&quot;local&quot;
         *              ref-name=&quot;ejb/<xsl:value-of select="$themeName"/>.IdManager&quot;
         * @ejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>&quot;
         *              view-type=&quot;local&quot;
         *              ref-name=&quot;ejb/MyEntityBeans&quot;<!--
         --><xsl:if test="local-name()='relationClass'"><xsl:for-each select="//objectClass"><xsl:variable name="anyClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable>
           * @ejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$anyClass"/>&quot;
           *              view-type=&quot;local&quot;
           *              ref-name=&quot;ejb/<xsl:value-of select="$anyClass"/>&quot;<!--
           --></xsl:for-each></xsl:if><!--
         
         
         --><xsl:if test="local-name()='relationClass'"><xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
           * @xejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$srcClass"/>&quot;
           *              view-type=&quot;local&quot;
           *              ref-name=&quot;ejb/<xsl:value-of select="$srcClass"/>&quot;<!--
           --></xsl:for-each><xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
             * @xejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$destClass"/>&quot;
             *              view-type=&quot;local&quot;
             *              ref-name=&quot;ejb/<xsl:value-of select="$destClass"/>&quot;<!--
             --></xsl:for-each></xsl:if>
           */
         
         public class <xsl:value-of select="$objectClass"/>SessionBean implements SessionBean
         {
           private SessionContext beanCtx=null;
           private IdManagerLocal myIdManager;
           private VersionLocal myVersion;
           private <xsl:value-of select="$objectClass"/>LocalHome myObjectLocalHome;

           // constants:
           public final static String[] simplePropertyKeys={
           <xsl:for-each select="simpleProperty">
             &quot;<xsl:value-of select="@key"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] simplePropertyNames={
           <xsl:for-each select="simpleProperty">
             &quot;<xsl:value-of select="name"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] simplePropertyDescriptions={
           <xsl:for-each select="simpleProperty">
             &quot;<xsl:value-of select="description"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static Class[] simplePropertyClasses={
           <xsl:for-each select="simpleProperty"><xsl:value-of select="type"/>.class<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};


           public final static String[] simplePropertyFormats={
           <xsl:for-each select="simpleProperty"><xsl:choose><xsl:when test="format">
                 &quot;<xsl:value-of select="format"/>&quot;                            
               </xsl:when><xsl:otherwise>
                 &quot;d.M.y H:m:s&quot;
               </xsl:otherwise></xsl:choose><xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
 
           public final String[] chartPropertyKeys={
           <xsl:for-each select="chartProperty">
             &quot;<xsl:value-of select="@key"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] chartPropertyNames={
           <xsl:for-each select="chartProperty">
             &quot;<xsl:value-of select="name"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] chartPropertyDescriptions={
           <xsl:for-each select="chartProperty">
             &quot;<xsl:value-of select="description"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};

           public final String[] vectorSetKeys={
           <xsl:for-each select="vectorSet">
             &quot;<xsl:value-of select="@key"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] vectorSetNames={
           <xsl:for-each select="vectorSet">
             &quot;<xsl:value-of select="name"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           public final static String[] vectorSetDescriptions={
           <xsl:for-each select="vectorSet">
             &quot;<xsl:value-of select="description"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>};
           
           <xsl:if test="local-name()='relationClass'">
             // Definition for RelationLocalHome-Interfaces (of all Objects)
             <xsl:for-each select="//objectClass"><xsl:variable name="anyObjectClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable>
               private <xsl:value-of select="$anyObjectClass"/>LocalHome my<xsl:value-of select="$anyObjectClass"/>LocalHome;
             </xsl:for-each>
           </xsl:if><xsl:if test="local-name()='relationClass'">
             //Relations:       
             public final static String forwardLabel=&quot;<xsl:value-of select="forwardLabel"/>&quot;;
           public final static String backwardLabel=&quot;<xsl:value-of select="backwardLabel"/>&quot;;
           
           /**
           * @ejb:interface-method
           */
           public String returnForwardLabel()
           {
            return forwardLabel;
           }
           /**
           * @ejb:interface-method
           */
           public String returnBackwardLabel()
           {
            return backwardLabel;
           }
           
           public final static String[] relationSourceKeys=
             {
             <xsl:for-each select="fromObjectClass">
               &quot;<xsl:value-of select="@ref"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
           </xsl:for-each>
           };
           public final static String[] relationDestinationKeys=
           {
           <xsl:for-each select="toObjectClass">
           &quot;<xsl:value-of select="@ref"/>&quot;<xsl:if test="position() != last()">,</xsl:if>
         </xsl:for-each>
         };

         /**
         * @ejb:interface-method
         */
         public String[] relationSourceKeys()
         {
          return relationSourceKeys;
         }
         /**
         * @ejb:interface-method
         */
         public String[] relationDestinationKeys()
         {
          return relationDestinationKeys;
         }
       </xsl:if><xsl:if test="local-name()='objectClass'">
           //Relations:
             <xsl:variable name="objectKey" select="@ID"/>             
             public final static String[] forwardRelations=
             {
             <xsl:for-each select="//fromObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
             &quot;<xsl:value-of select="$relationClass"/>&quot;<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>
           };
           public final static String[] backwardRelations=
           {
           <xsl:for-each select="//toObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
           &quot;<xsl:value-of select="$relationClass"/>&quot;<xsl:if test="position() != last()">,</xsl:if></xsl:for-each>
         };
       </xsl:if><!--       
            <xsl:for-each select="chartProperty">
              <xsl:variable name="propName">
                <xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>        
              </xsl:variable>
              /**
              * @ejb:interface-method
              */
              public ChartProperty get<xsl:value-of select="$propName"/>(int pos) throws FinderException
              {
              return getObject(pos).get<xsl:value-of select="$propName"/>();
              }
              
              /**
              * @ejb:interface-method
              */
              public void set<xsl:value-of select="$propName"/>(int pos,ChartProperty value) throws FinderException
              {
              getObject(pos).set<xsl:value-of select="$propName"/>(value);
              }
            </xsl:for-each>
            -->

              /**
              * @ejb:interface-method
              */
              public Vector getVectorSets(Object primKey) throws javax.ejb.FinderException
              {
              Vector result=new Vector();
               for(int i=0;i <![CDATA[ < ]]><xsl:value-of select="count(vectorSet)"/>;i++)
                result.add(getVectorSet(primKey,i));
               return result;
              }

              /**
              * @ejb:interface-method
              */
              public void setVectorSets(Object primKey,Vector vectorSets) throws javax.ejb.FinderException
              {            
               System.out.println(&quot;SessionJavaBean: setVectorSets... size&quot;+vectorSets.size());
               for(int i=0;i <![CDATA[ < ]]><xsl:value-of select="count(vectorSet)"/>;i++)
                setVectorSet(primKey,i,vectorSets.elementAt(i));
              }

              /**
              * @ejb:interface-method
              */
              public Object getVectorSet(Object primKey,int pos) throws javax.ejb.FinderException
              {
                <xsl:for-each select="vectorSet"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
                  if(pos==<xsl:value-of select="position()-1"/>)
                   return getMyObject(primKey).get<xsl:value-of select="$propName"/>();
                </xsl:for-each>
                return null;
              }

              /**
              * @ejb:interface-method
              */
              public void setVectorSet(Object primKey,int pos,Object value) throws javax.ejb.FinderException
              {
                System.out.println(&quot;setVectorSet Nr&quot;+pos);
                switch(pos)
                {
                <xsl:for-each select="vectorSet"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
                  case <xsl:value-of select="position()-1"/>:
                  System.out.println(&quot;SessionJavaBean.setVectorSet <xsl:value-of select="$propName"/>...&quot;);
                  getMyObject(primKey).set<xsl:value-of select="$propName"/>((<xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/>)value);
                  break;
                </xsl:for-each>
                 default:
                  break;
                }
              }

         // Meta-information:
         /**
         * @ejb:interface-method
         */
         public boolean isRelation()
         {
         <xsl:choose><xsl:when test="relationClass">
             return true;
           </xsl:when><xsl:otherwise>
             return false;
           </xsl:otherwise></xsl:choose>
         }

         <xsl:if test="local-name()='objectClass'">
           /**
           * @ejb:interface-method
           */
           public String[] backwardRelations()
           {
           return backwardRelations;
           }       
           
           /**
           * @ejb:interface-method
           */
           public String[] forwardRelations()
           {
           return forwardRelations;
           }               

           /**
           * @ejb:interface-method
           */
           public Vector returnForwardRelations(Object primKey) throws javax.ejb.FinderException
           {
            return getMyObject(primKey).returnForwardRelations();
           }

           /**
           * @ejb:interface-method
           */
           public Vector returnBackwardRelations(Object primKey) throws javax.ejb.FinderException
           {
            return getMyObject(primKey).returnBackwardRelations();
           }
         </xsl:if>
         
         /**
         * @ejb:interface-method
         */
         public String name()
         {
          return &quot;<xsl:value-of select="name"/>&quot;;
         }

         /**
         * @ejb:interface-method
         */         
         public String description()
         {
          return &quot;<xsl:value-of select="description"/>&quot;;
         }

         /**
         * @ejb:interface-method
         */
         public String key()
         {
          return &quot;<xsl:value-of select="@ID"/>&quot;;
         }
         
         /**
         * @ejb:interface-method
         */
         public String[] simplePropertyKeys()
         {
          return simplePropertyKeys;
         }
         /**
         * @ejb:interface-method
         */
         public String[] chartPropertyKeys()
         {
          return chartPropertyKeys;
         }
         /**
         * @ejb:interface-method
         */
         public String[] vectorSetKeys()
         {
          return vectorSetKeys;
         }
         /**
         * @ejb:interface-method
         */
         public String[] simplePropertyNames()
         {
           return simplePropertyNames;
         }
         /**
         * @ejb:interface-method
         */
         public String[] chartPropertyNames()
         {
           return chartPropertyNames;
         }
         /**
         * @ejb:interface-method
         */
         public String[] vectorSetNames()
         {
           return vectorSetNames;
         }
         
         /**
         * @ejb:interface-method
         */
         public String[] simplePropertyDescriptions()
         {
          return simplePropertyDescriptions;
         }
         /**
         * @ejb:interface-method
         */
         public String[] chartPropertyDescriptions()
         {
          return chartPropertyDescriptions;
         }
         /**
         * @ejb:interface-method
         */
         public String[] vectorSetDescriptions()
         {
          return vectorSetDescriptions;
         }

         /**
         * @ejb:interface-method
         */
         public String[] simplePropertyFormats()
         {
          return simplePropertyFormats;
         }

         /**
         * @ejb:interface-method
         */
         public Class[] simplePropertyClasses()
         {
          return simplePropertyClasses;
         }

         // BasePoints          
         /**
         * @ejb:interface-method
         */
         public boolean hasSymbol()
         {
         <xsl:choose><xsl:when test="symbol">
             return true;
           </xsl:when><xsl:otherwise>
             return false;
           </xsl:otherwise></xsl:choose>
         }

         // getter and setter
         <xsl:if test="local-name()='objectClass'">
           // Basepoint-methods, defined in ObjectSession-interface
           /**
           * @ejb:interface-method
           */
           public void setBasePointX(Object primKey,Double x) throws javax.ejb.FinderException
           {
           <xsl:choose><xsl:when test="symbol">
               getMyObject(primKey).setBasePointX(x);
             </xsl:when><xsl:otherwise>
               System.out.println(&quot;:-/ setBasePoint, althought Object has no symbol&quot;);
             </xsl:otherwise></xsl:choose>
           }
           
           /**
           * @ejb:interface-method
           */
           public Double getBasePointX(Object primKey) throws javax.ejb.FinderException
           {
           <xsl:choose><xsl:when test="symbol">
               return getMyObject(primKey).getBasePointX();
             </xsl:when><xsl:otherwise>
               return null;
             </xsl:otherwise></xsl:choose>
           }

           /**
           * @ejb:interface-method
           */
           public void setBasePointY(Object primKey,Double y) throws javax.ejb.FinderException
           {
           <xsl:choose><xsl:when test="symbol">
               getMyObject(primKey).setBasePointY(y);
             </xsl:when><xsl:otherwise>
               System.out.println(&quot;:-/ setBasePoint, althought Object has no symbol&quot;);
             </xsl:otherwise></xsl:choose>
           }
           
           /**
           * @ejb:interface-method
           */
           public Double getBasePointY(Object primKey) throws javax.ejb.FinderException
           {
           <xsl:choose><xsl:when test="symbol">
               return getMyObject(primKey).getBasePointY();
             </xsl:when><xsl:otherwise>
               return null;
             </xsl:otherwise></xsl:choose>
           }
         </xsl:if><xsl:for-each select="simpleProperty"><xsl:variable name="fieldName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
           // Todo: zusaetzlich nach Version selektieren
           /**
           * @ejb:interface-method
           */
           public Vector findBy<xsl:value-of select="$fieldName"/>(Object value) throws javax.ejb.FinderException
           {
            Collection col=myObjectLocalHome.findBy<xsl:value-of select="$fieldName"/>((<xsl:value-of select="type"/>)value);
            Vector resultVector=new Vector();
            Iterator it=col.iterator();
            while(it.hasNext())
            {
             resultVector.add(((ElementLocal)it.next()).getId());
            }
            return resultVector;
           }
          </xsl:for-each>

          
          /**
          * @ejb:interface-method
          */
          public void toXML(String fileName) throws java.io.IOException
          {
            Collection col=myVersion.get<xsl:value-of select="$objectClass"/>();
            PrintWriter out=new PrintWriter(new FileWriter(fileName,true));
            Tools.genXmlTag(out,&quot;table&quot;,&quot;key&quot;,&quot;<xsl:value-of select="@ID"/>&quot;);
            Iterator it=col.iterator();
            while(it.hasNext())
            {
             ((<xsl:value-of select="$objectClass"/>Local)it.next()).toXML(out);
            } 
           Tools.genXmlTag(out,&quot;/table&quot;);
           out.close();  
          }

          /**
          * @ejb:interface-method
          */
          public void loadGisTransferObject(Object primKey,GisTransferObject transferObject) throws javax.ejb.FinderException
          {
           getMyObject(primKey).loadGisTransferObject(transferObject);
          }


         <xsl:for-each select="simpleProperty"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
           /**
           * @ejb:interface-method
           */
           public <xsl:value-of select="type"/> get<xsl:value-of select="$propName"/>(Object primKey) throws javax.ejb.FinderException
           {
            return getMyObject(primKey).get<xsl:value-of select="$propName"/>();
           }                      
           /**
           * @ejb:interface-method
           */
           public void set<xsl:value-of select="$propName"/>(Object primKey,<xsl:value-of select="type"/> value) throws javax.ejb.FinderException
           {
             getMyObject(primKey).set<xsl:value-of select="$propName"/>(value);
           }  

         </xsl:for-each>
                  
         // PropertySize...
         /**
         * @ejb:interface-method
         */
         public int getSimplePropertySize()
         {
          return <xsl:value-of select="count(simpleProperty)"/>;
         }
         /**
         * @ejb:interface-method
         */
         public int getChatPropertySize()
         {
          return <xsl:value-of select="count(chatProperty)"/>;
         }
         /**
         * @ejb:interface-method
         */
         public int getVectorSetSize()
         {
          return <xsl:value-of select="count(vectorSet)"/>;
         }

         // PropertyNames
         /**
         * @ejb:interface-method
         */
         public String[] getSimplePropertyNames()
         {
          return simplePropertyNames;
         } 
         /**
         * @ejb:interface-method
         */
         public String[] getChartPropertyNames()
         {
          return chartPropertyNames;
         }
         /**
         * @ejb:interface-method
         */
         public String[] getVectorSetNames()
         {
          return vectorSetNames;
         }
         // simplePropertyClasses
         /**
         * @ejb:interface-method
         */
         public Class[] getSimplePropertyClasses()
         {
          return simplePropertyClasses;
         }

         // Property- setter/getter
         /**
         * @ejb:interface-method
         */
         public Object getSimplePropertyValue(Object primKey,int colIndex) throws javax.ejb.FinderException
         {
          switch(colIndex)
            {
             <xsl:for-each select="simpleProperty"><xsl:variable name="propClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
               case <xsl:value-of select="position()-1"/>:
               return get<xsl:value-of select="$propClass"/>(primKey);
             </xsl:for-each>
             default:
             return &quot;#&quot;;
            }
         }           
             /**
             * @ejb:interface-method
             */
             public Hashtable getSimplePropertyRows(Vector primKeys) throws javax.ejb.FinderException
             {
             Hashtable result=new Hashtable();
             
             for(int n=0;n <![CDATA[ < ]]> primKeys.size();n++)
             {
              Object primKey=primKeys.elementAt(n);
              result.put(primKey,getSimplePropertyRow(primKey));
             }
             return result;
             }
            
             /**
             * @ejb:interface-method
             */
             public Vector getSimplePropertyRow(Object primKey) throws javax.ejb.FinderException
             {
              Vector resultRow=new Vector();
              for(int i=0;i <![CDATA[ < ]]> getSimplePropertySize();i++)
               resultRow.add(getSimplePropertyValue(primKey,i));
              return resultRow;
             }

             /**
            * @ejb:interface-method
            */
            public void setSimplePropertyValue(Object primKey,int colIndex,Object value) throws javax.ejb.FinderException 
            {
            switch(colIndex)
            {
            <xsl:for-each select="simpleProperty"><xsl:variable name="propClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
              case <xsl:value-of select="position()-1"/>:
              set<xsl:value-of select="$propClass"/>(primKey,(<xsl:value-of select="type"/>)value);
              break;
            </xsl:for-each>
            default:
            break;
            }            
            return;
            }



            // Create-Methods
           <xsl:if test="local-name()='objectClass'">
           /**
           * @ejb:interface-method
           */
           public Object createObject() throws CreateException
           {
            <xsl:value-of select="$objectClass"/>Local newObject = myObjectLocalHome.create(myIdManager.useId(),myVersion);
            return newObject.getPrimaryKey();
           }
          </xsl:if><xsl:if test="local-name()='relationClass'">
            /**
            * @ejb:interface-method
            */
            public Object createRelation(String srcKey,Object srcId,String destKey,Object destId) throws CreateException
            {
            Object src=null;
                        
            <xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
              if(&quot;<xsl:value-of select="@ref"/>&quot;.equals(srcKey))
              try
              {
              src=my<xsl:value-of select="$srcClass"/>LocalHome.findByPrimaryKey((Integer)srcId);
              }
              catch(FinderException e)
              {
               throw new CreateException(e.getMessage()+&quot;maybe type of sourceObject is not allowed in this relationship&quot;);
              }
            </xsl:for-each>
            if(src==null)
              throw new CreateException(&quot;Type of sourceObject is not allowed in this relationship&quot;);

            Object dest=null;
            <xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
              if(&quot;<xsl:value-of select="@ref"/>&quot;.equals(destKey))
              try
              {
                dest=my<xsl:value-of select="$destClass"/>LocalHome.findByPrimaryKey((Integer)destId);
              }
              catch(FinderException e)
              {
                throw new CreateException(e.getMessage()+&quot;maybe type of destinationObject is not allowed in this relationship&quot;);
              }
            </xsl:for-each>
            if(dest==null)
             throw new CreateException(&quot;Type of destinationObject is not allowed in this relationship&quot;);
            
            <xsl:value-of select="$relationClass"/>Local newRelation = myObjectLocalHome.create(myIdManager.useId(),myVersion,src,dest);
            return newRelation.getPrimaryKey();
            }
          </xsl:if><xsl:if test="local-name()='relationClass'">
            

          /**
          * @ejb:interface-method
          */
          public Vector getRelationVector(Object rId) throws javax.ejb.FinderException
          {
            System.out.println(name()+&quot;Session: getRelationsVector...&quot;);
           <xsl:value-of select="$objectClass"/>Local relation=getMyObject(rId);
            System.out.println(name()+&quot;Session: found Relation #&quot;+relation.getId());
           Vector result=new Vector();
           Object srcId =null;
           String srcKey=null;
           Object destId =null;
           String destKey=null;

            <xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
              if(relation.getSrc<xsl:value-of select="$srcClass"/>()!=null)
              {
                srcId=relation.getSrc<xsl:value-of select="$srcClass"/>().getId();
                srcKey=&quot;<xsl:value-of select="@ref"/>&quot;;
              }
           </xsl:for-each>
            <xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>

              if(relation.getDest<xsl:value-of select="$destClass"/>()!=null)
              {
                destId=relation.getDest<xsl:value-of select="$destClass"/>().getId();              
                destKey=&quot;<xsl:value-of select="@ref"/>&quot;;
              }
           </xsl:for-each>
            result.add(srcId);
            result.add(srcKey);
            result.add(destId);
            result.add(destKey);
            System.out.println(name()+&quot;Session: RelationsVector is:&quot;);
            System.out.println(name()+&quot;Session: srcID&quot;+srcId.toString()+&quot;  &quot;+srcKey);
            System.out.println(name()+&quot;Session: destID&quot;+destId.toString()+&quot;   &quot;+destKey);
            return result;
           }
          </xsl:if>

         /**
         * @ejb:interface-method
         */
         public void removeObject(Integer id) throws RemoveException, FinderException
         {
           myObjectLocalHome.remove(id);
         }

         /**
         * @ejb:interface-method
         */
          public Vector getPrimaryKeyList() //throws Exception
         {

            Collection col=myVersion.get<xsl:value-of select="$objectClass"/>();
            Vector primaryKeyList=new Vector();
            System.out.println(&quot;ObjectSession:  got myObjects (&quot;+col.size()+&quot; elements) ... &quot;);
            Iterator it=col.iterator();
            while(it.hasNext())
            {
              primaryKeyList.add(((<xsl:value-of select="$objectClass"/>Local) it.next()).getPrimaryKey());
            } 
            return primaryKeyList;
         }

          <!--
               /**
               * @ejb:interface-method
               */
               public Vector getPrimaryKeyListOrdered(int colIndex) throws javax.ejb.FinderException //throws Exception
               {
               Collection col=null;
               switch(colIndex)
               {
               case 0:
               break;  //primKey
               <xsl:for-each select="simpleProperty">
                 <xsl:variable name="propClass">
                   <xsl:call-template name="toUpperCase">
                     <xsl:with-param name="word" select="@key"/>
                   </xsl:call-template>
                 </xsl:variable>
                 case <xsl:value-of select="position()"/>:
                 col=myObjectLocalHome.findOrderedBy<xsl:value-of select="$propClass"/>();
                 break;
               </xsl:for-each>
               default:
               throw(new FinderException("unable to return ordered List"));           
               }
               primaryKeyList=new Vector();
               System.out.println("ObjectSession:  got sorted Objects ("+col.size()+" elements) ... ");
               Iterator it=col.iterator();
               while(it.hasNext())
               {
               primaryKeyList.add(((<xsl:value-of select="$objectClass"/>Local) it.next()).getPrimaryKey());
               }           
               return primaryKeyList;
               }
               -->

         /**
         * @ejb:interface-method
         */
         public int getSize()
         {
           Collection col=myVersion.get<xsl:value-of select="$objectClass"/>();
           return col.size();
         }

          /**
          * @ejb:interface-method
          */
          public String getInfo(Object primKey) throws FinderException
          {
           return getMyObject(primKey).getInfo();
          }

          /**
          * @ejb:interface-method
          */
          public String getInfoForAll(Object primKey) throws FinderException
          {
           return getMyObject(primKey).getInfo();
          }


          /**
          * Bean-logic
          */
          private <xsl:value-of select="$objectClass"/>Local getMyObject(Object primKey) throws FinderException
          {
            return myObjectLocalHome.findByPrimaryKey((Integer)primKey);
          }

         public void setSessionContext(SessionContext ctx)
         {
           this.beanCtx=ctx;
         }
         
         public void ejbCreate() throws CreateException
         {	
         // fake
         }
               
         /**
         * @ejb:create-method
         */
         public void ejbCreate(Object versionId) throws CreateException
         {	
           Hashtable env=new Hashtable();
           env.put(&quot;java.naming.factory.initial&quot;,&quot;org.jnp.interfaces.NamingContextFactory&quot;);
           env.put(&quot;java.naming.factory.url.pkgs&quot;,&quot;org.jboss.naming:org.jnp.interfaces&quot;);
           env.put(&quot;java.naming.provider.url&quot;,&quot;localhost&quot;);
           System.out.println(&quot;context... &quot;);
           try
           { 
             InitialContext ctx=new InitialContext(env); // env

             System.out.println(&quot;ObjectSession: request my VersionLocalHome... &quot;);
             VersionLocalHome versionLocalHome =(VersionLocalHome)ctx.lookup(&quot;java:comp/env/ejb/Versions&quot;);
             System.out.println(&quot;ObjectSession:  got my VersionLocalHome&quot;);

             System.out.println(&quot;ObjectSession: request my VersionEntity... &quot;);
             myVersion=versionLocalHome.findByPrimaryKey((Integer) versionId);
             System.out.println(&quot;ObjectSession:  got my VersionEntity&quot;);

             System.out.println(&quot;ObjectSession: request my EntityBeanLocalHome... &quot;);
             myObjectLocalHome=(<xsl:value-of select="$objectClass"/>LocalHome) ctx.lookup(&quot;java:comp/env/ejb/MyEntityBeans&quot;);
             System.out.println(&quot;ObjectSession:  got my EntityBeanLocalHome&quot;);

         <xsl:if test="local-name()='relationClass'">
           // get RelationLocalHome-methods (get LocalHomes of all objects)
           <xsl:for-each select="//objectClass"><xsl:variable name="anyObjectClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable>                          
             System.out.println(&quot;RelationSession: request my <xsl:value-of select="$anyObjectClass"/>LocalHome... &quot;);
             my<xsl:value-of select="$anyObjectClass"/>LocalHome =(<xsl:value-of select="$anyObjectClass"/>LocalHome) ctx.lookup(&quot;java:comp/env/ejb/<xsl:value-of select="$anyObjectClass"/>&quot;);
             System.out.println(&quot;RelationSession:  got my <xsl:value-of select="$anyObjectClass"/>LocalHome&quot;);
           </xsl:for-each>
         </xsl:if>
         

             System.out.println(&quot;ObjectSession: request the idManagerLocalHome&quot;);
             IdManagerLocalHome idManagerLocalHome =(IdManagerLocalHome)ctx.lookup(&quot;java:comp/env/ejb/<xsl:value-of select="$themeName"/>.IdManager&quot;);
             System.out.println(&quot;ObjectSession:  got myIdManagerLocalHome&quot;);

             try
             {
               System.out.println(&quot;ObjectSession: request my IdManagerEntity...&quot;);
               myIdManager=idManagerLocalHome.findByPrimaryKey(&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>&quot;);
               System.out.println(&quot;ObjectSession:  got myIdManagerEntity&quot;);
             }
             catch(FinderException e)
             {
               System.out.println(e.getMessage());
               System.out.println(&quot;  ObjectSession: maybe IdManagerEntity has not been created for this table, try to create ...&quot;);
               idManagerLocalHome.create(&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>&quot;);
               System.out.println(&quot;  ObjectSession: created IdManagerEntity&quot;);
 
               System.out.println(&quot;  ObjectSession: request my IdManagerEntity again&quot;);
               myIdManager=idManagerLocalHome.findByPrimaryKey(&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$objectClass"/>&quot;);
               System.out.println(&quot;  ObjectSession:  yes, got myIdManagerEntity now&quot;);
             }
           }
           catch(FinderException e)
           {
             throw new CreateException(e.getMessage());
           }         
           catch(NamingException e)
           {
             throw new CreateException(e.getMessage());
           }           
         }
    
         public void ejbActivate() // prepare for activate
         {
           System.out.println(&quot;aktivate...&quot;);
         }
    
         public void ejbRemove() // free recources
         {    
           myIdManager=null;
           myVersion=null;
           myObjectLocalHome=null;
           System.out.println(&quot;remove...&quot;);
         }

         public void ejbPassivate() // prepare for passivate
         {
           System.out.println(&quot;passivate...&quot;);
         }
                               
         }
  </xsl:template>
<!--===============================================================================-->
<!--                                                                               -->
<!--                             EntityBean                                        -->
<!--                                                                               -->
<!--===============================================================================-->
  <xsl:template mode="bean" match="theme//child::*[self::objectClass or self::relationClass]"><xsl:variable name="name"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable><xsl:variable name="objectClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ID"/></xsl:call-template></xsl:variable><xsl:variable name="relationClass"><xsl:value-of select="$objectClass"/></xsl:variable>
    package <xsl:value-of select="$package"/>;
    
    /**
    * generated java-code for BEAN:
    * <xsl:value-of select="description"/>
    */
    
    import javax.ejb.CreateException;
    import javax.ejb.EntityBean;
    import javax.ejb.EntityContext;
    import javax.ejb.FinderException;

    import java.io.PrintWriter;
    
    import java.util.Vector;
    import java.util.ArrayList;
    import java.util.Collection;
    import java.util.Iterator;  


        import java.text.ParseException;
        import java.text.SimpleDateFormat;
        import java.text.DateFormat;
        import java.util.Date;

    import de.tuhh.wb.javagis.model.ElementLocal;
    import de.tuhh.wb.javagis.model.VersionLocal;
    import de.tuhh.wb.javagis.xml.GisTransferObject;
    import de.tuhh.wb.javagis.model.Tools;    
    import <xsl:value-of select="$package"/>.vectorsets.*;         

    <!--  
         * @ejb:finder signature="java.util.Collection findOrdered()"
         * @jboss:finder-query name="findOrdered" query="foo = {1}" order="foo,bar"
         * @ejb:finder signature="Collection findLargeAccounts(int balance)" role-name="Teller,IRS"
         * @jboss:finder-query name="findLargeAccounts" query="$1 = $1" order="balance" read-ahead="true"
         -->
    /**
    * @ejb:bean   name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>&quot;
    *             jndi-name=&quot;ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>&quot;
    *             local-jndi-name=&quot;<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>&quot;    
    *             view-type=&quot;local&quot;
    *             type=&quot;CMP&quot;
    *             cmp-version=&quot;2.x&quot;
    *             schema=&quot;<xsl:value-of select="$themeName"/>_<xsl:value-of select="$name"/>&quot;
    *             primkey-field=&quot;id&quot;
    * @ejb:pk generate=&quot;false&quot; class=&quot;java.lang.Integer&quot;
    * @ejb:ejb-ref ejb-name=&quot;<xsl:value-of select="$themeName"/>.Version&quot; view-type=&quot;local&quot;
    <xsl:if test="local-name()='objectClass'">
      * @ejb:interface local-extends=&quot;de.tuhh.wb.javagis.model.ObjectLocal&quot;
    </xsl:if><xsl:if test="local-name()='relationClass'">
      * @ejb:interface local-extends=&quot;de.tuhh.wb.javagis.model.RelationLocal&quot;
    </xsl:if><!--
    --><xsl:for-each select="simpleProperty"><xsl:variable name="fieldName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
    * @ejb:finder signature=&quot;java.util.Collection findBy<xsl:value-of select="$fieldName"/>(<xsl:value-of select="type"/><xsl:value-of select="' '"/><xsl:value-of select="@key"/>)&quot; 
    * query=&quot;SELECT OBJECT(o) FROM <xsl:value-of select="$themeName"/>_<xsl:value-of select="$name"/> AS o WHERE o.<xsl:value-of select="@key"/> =?1&quot;
    <!--
         * @ejb:finder signature="java.util.Collection findOrderedBy<xsl:value-of select="$fieldName"/>()"
         * @jboss:finder-query name="findOrderedBy<xsl:value-of select="$fieldName"/>" query="1-1" order="<xsl:value-of select="@key"/>" read-ahead="true"
         --></xsl:for-each>
    */
  
    public abstract class <xsl:value-of select="$name"/>Bean implements EntityBean
    {
    private transient EntityContext ctx;
    
    // relations...
    /**
    * @ejb:interface-method
    * @ejb:relation name=&quot;Versioning-of-<xsl:value-of select="$name"/>&quot;
    *               role-name=&quot;<xsl:value-of select="$name"/>-has-Version&quot;
    *               cascade-delete=&quot;yes&quot;
    * @jboss:relation fk-constraint=&quot;false&quot;
    *                 related-pk-field=&quot;id&quot;
    *                 fk-column=&quot;version&quot;
    */
    public abstract VersionLocal getVersion();
    <xsl:if test="local-name()='relationClass'"><xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name=&quot;<xsl:value-of select="$srcClass"/>_<xsl:value-of select="$relationClass"/>&quot;
        *               role-name=&quot;<xsl:value-of select="$relationClass"/>_to_<xsl:value-of select="$srcClass"/>&quot;
        *               cascade-delete=&quot;yes&quot;
        * @jboss:relation fk-constraint=&quot;false&quot;
        *                 related-pk-field=&quot;id&quot;
        *                 fk-column=&quot;relTo<xsl:value-of select="$srcClass"/>&quot;
        */
        public abstract <xsl:value-of select="$srcClass"/>Local getSrc<xsl:value-of select="$srcClass"/>();

        /**
        * @ejb:interface-method
        */
        public abstract void setSrc<xsl:value-of select="$srcClass"/>(<xsl:value-of select="$srcClass"/>Local source);
      </xsl:for-each><xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name=&quot;<xsl:value-of select="$relationClass"/>_<xsl:value-of select="$destClass"/>&quot;
        *               role-name=&quot;<xsl:value-of select="$relationClass"/>_to_<xsl:value-of select="$destClass"/>&quot;
        * @jboss:relation fk-constraint=&quot;false&quot;
        *                 related-pk-field=&quot;id&quot;
        *                 fk-column=&quot;relFrom<xsl:value-of select="$destClass"/>&quot;
        */
        public abstract <xsl:value-of select="$destClass"/>Local getDest<xsl:value-of select="$destClass"/>();

        /**
        * @ejb:interface-method
        */
        public abstract void setDest<xsl:value-of select="$destClass"/>(<xsl:value-of select="$destClass"/>Local destination);
      </xsl:for-each></xsl:if><xsl:if test="local-name()='objectClass'"><xsl:variable name="objectKey" select="@ID"/><xsl:for-each select="//fromObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name=&quot;<xsl:value-of select="$objectClass"/>_<xsl:value-of select="$relationClass"/>&quot;
        *               role-name=&quot;<xsl:value-of select="$objectClass"/>_to_<xsl:value-of select="$relationClass"/>&quot;
        */
        public abstract Collection getRelTo<xsl:value-of select="$relationClass"/>();        

        /**
        * @ejb:interface-method
        */
        public abstract void setRelTo<xsl:value-of select="$relationClass"/>(Collection col);        
      </xsl:for-each><xsl:if test="local-name()='objectClass'"><xsl:variable name="objectKey" select="@ID"/>
        /**
        * @ejb:interface-method
        */
        public Vector returnForwardRelations()
        {
         Vector result=new Vector();
         Collection collection;

         <xsl:for-each select="//fromObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
           collection=getRelTo<xsl:value-of select="$relationClass"/>();
           if(collection.size()&gt;0)
           {
           Vector relation=new Vector();
           Vector idList=new Vector();
           Iterator it=collection.iterator();
           while(it.hasNext())
           {
           idList.add(((ElementLocal)it.next()).getId());
           }                     
           relation.add(&quot;<xsl:value-of select="../@ID"/>&quot;); // key
           relation.add(idList); // IDs
           result.add(relation);
           }
         </xsl:for-each>      
         return result;
        }

        /**
        * @ejb:interface-method
        */
        public Vector returnBackwardRelations()
        {
         Vector result=new Vector();
         Collection collection;

         <xsl:for-each select="//toObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
           collection=getRelFrom<xsl:value-of select="$relationClass"/>();
           if(collection.size()&gt;0)
           {
            Vector relation=new Vector();
            Vector idList=new Vector();
            Iterator it=collection.iterator();
            while(it.hasNext())
            {
             idList.add(((ElementLocal)it.next()).getId());
            }                     
           relation.add(&quot;<xsl:value-of select="../@ID"/>&quot;); // key
            relation.add(idList); // IDs
            result.add(relation);
           }
         </xsl:for-each>      
         return result;
        }
      </xsl:if><xsl:for-each select="//toObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name=&quot;<xsl:value-of select="$relationClass"/>_<xsl:value-of select="$objectClass"/>&quot;
        *               role-name=&quot;<xsl:value-of select="$objectClass"/>_to_<xsl:value-of select="$relationClass"/>&quot;
        */
        public abstract Collection getRelFrom<xsl:value-of select="$relationClass"/>();        

        /**
        * @ejb:interface-method
        */
        public abstract void setRelFrom<xsl:value-of select="$relationClass"/>(Collection col);        
      </xsl:for-each></xsl:if>
      
    /**
    * @ejb:interface-method
    */
    public abstract void setVersion(VersionLocal version);
    
    // PrimaryKey...
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

    <xsl:if test="symbol">
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setBasePointX(Double x);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getBasePointX();

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setBasePointY(Double y);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getBasePointY();
    </xsl:if><xsl:if test="geometry">
      // Min
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setGeoBoxMinX(Double x);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getGeoBoxMinX();

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setGeoBoxMinY(Double y);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getGeoBoxMinY();

      // Max
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setGeoBoxMaxX(Double x);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getGeoBoxMaxX();

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void setGeoBoxMaxY(Double y);
    
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract Double getGeoBoxMaxY();
      <!--
        /**
        * @ejb:interface-method
        * @ejb:persistent-field
        */
        public abstract void setGeometry(BasePoints geometry);
        
        /**
        * @ejb:interface-method
        * @ejb:persistent-field
        */
        public abstract BasePoints getGeometry();
-->      
      </xsl:if>

    // simpleProperty..
    <xsl:for-each select="simpleProperty"><xsl:variable name="fieldName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable><xsl:variable name="fieldType"><xsl:value-of select="type"/></xsl:variable>
      
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void set<xsl:value-of select="$fieldName"/>(<xsl:value-of select="$fieldType"/> value);

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract <xsl:value-of select="$fieldType"/> get<xsl:value-of select="$fieldName"/>();
    </xsl:for-each>

    
    // chartProperties...
    <xsl:for-each select="chartProperty"><xsl:variable name="chartName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
      
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void set<xsl:value-of select="$chartName"/>(ChartProperty value);

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract ChartProperty get<xsl:value-of select="$chartName"/>();
    </xsl:for-each>

    
    // vectorSet...
    <xsl:for-each select="vectorSet"><xsl:variable name="vectorSetName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
      
      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract void set<xsl:value-of select="$vectorSetName"/>(<xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> value);

      /**
      * @ejb:interface-method
      * @ejb:persistent-field
      */
      public abstract <xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> get<xsl:value-of select="$vectorSetName"/>();
    </xsl:for-each>
  
    private Long date2Millis(Object date)
    {
     if(date==null)
      return null;
    else    
    {
     java.util.GregorianCalendar calendar=new java.util.GregorianCalendar();
     calendar.setTime((Date)date);
     return new Long(calendar.getTimeInMillis());
    }
    }

    /**
    * @ejb:interface-method
    */
    public void toXML(PrintWriter out)
    {
     Tools.genXmlTag(out,&quot;o&quot;,&quot;ID&quot;,String.valueOf(getId()));

    Object[] simpleProps=
    {
    <xsl:for-each select="simpleProperty"><xsl:variable name="Property"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable><xsl:choose><xsl:when test="type='java.util.Date'">
          date2Millis(get<xsl:value-of select="$Property"/>())
        </xsl:when><xsl:otherwise>
          get<xsl:value-of select="$Property"/>()
        </xsl:otherwise></xsl:choose><xsl:if test="position() != last()">,</xsl:if></xsl:for-each>};
    
    Tools.genXmlOpenTag(out,&quot;sp&quot;,<xsl:value-of select="$objectClass"/>SessionBean.simplePropertyKeys,simpleProps);
    
    // vectorSetsToXML:
    <xsl:for-each select="vectorSet"><xsl:variable name="vectorSetName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
      get<xsl:value-of select="$vectorSetName"/>().toXml(out);
    </xsl:for-each>
    Tools.genXmlTag(out,&quot;/sp&quot;);
    <xsl:if test="local-name()='relationClass'">

      Object srcId =null;
      String srcKey=null;
      Object destId =null;
      String destKey=null;

      <xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
        {
         srcId=getSrc<xsl:value-of select="$srcClass"/>().getId();
         srcKey=&quot;<xsl:value-of select="@ref"/>&quot;;
        }
      </xsl:for-each>
      <xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        
        if(getDest<xsl:value-of select="$destClass"/>()!=null)
        {
         destId=getDest<xsl:value-of select="$destClass"/>().getId();              
         destKey=&quot;<xsl:value-of select="@ref"/>&quot;;
        }
      </xsl:for-each>

      String[] attNames={&quot;srcKey&quot;,&quot;srcID&quot;,&quot;destKey&quot;,&quot;destID&quot;};
      String[] atts={srcKey,srcId.toString(),destKey,destId.toString()};
      Tools.genXml(out,&quot;rel&quot;,attNames,atts,&quot;&quot;);      
    </xsl:if>
    
     Tools.genXmlTag(out,&quot;/o&quot;);
    }

    /**
    * @ejb:interface-method
    */
    public void loadGisTransferObject(GisTransferObject transferObject)
    {
    // SimpleProperties:
    String value;
    java.util.GregorianCalendar calendar=new java.util.GregorianCalendar();
    <xsl:for-each select="simpleProperty"><xsl:variable name="Property"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>
      value=transferObject.getSimpleProperty(&quot;<xsl:value-of select="@key"/>&quot;);
      if(value!=null)
      {
      <xsl:choose><xsl:when test="type='java.util.Date'">
          try
          {
           calendar.setTimeInMillis(java.lang.Long.parseLong(value));
           set<xsl:value-of select="$Property"/>(calendar.getTime());
          }
          catch(Exception e)
          {
           System.out.println(&quot;problem parsing date (not in longformat?)&quot;);
           System.out.println(e.getMessage());
          }
       </xsl:when><xsl:otherwise>
         set<xsl:value-of select="$Property"/>(new <xsl:value-of select="type"/>(value));
       </xsl:otherwise></xsl:choose>
      }
    </xsl:for-each>
    
    // VectorSets:
    <xsl:for-each select="vectorSet"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>          
      // get vectorSet
      <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/> my<xsl:value-of select="$propName"/> = get<xsl:value-of select="$propName"/>();
      // set vectorSet
      my<xsl:value-of select="$propName"/>.loadFromGisTransferObject(transferObject);
      set<xsl:value-of select="$propName"/>(my<xsl:value-of select="$propName"/>);
    </xsl:for-each>
    }

    /**
    * @ejb:interface-method
    */
    public String getInfo()
    {
      String info=&quot;ID#&quot;+String.valueOf(getId());

    <xsl:if test="local-name()='objectClass'"><xsl:variable name="objectKey" select="@ID"/><xsl:for-each select="//fromObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
        info+=&quot; has &quot;+String.valueOf(getRelTo<xsl:value-of select="$relationClass"/>().size())+&quot; Relations to <xsl:value-of select="$relationClass"/>&quot;;
      </xsl:for-each><xsl:for-each select="//toObjectClass[@ref=$objectKey]"><xsl:variable name="relationClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@ID"/></xsl:call-template></xsl:variable>
        info+=&quot; has &quot;+String.valueOf(getRelFrom<xsl:value-of select="$relationClass"/>().size())+&quot; Relations from <xsl:value-of select="$relationClass"/>&quot;;
      </xsl:for-each></xsl:if><xsl:if test="local-name()='relationClass'"><xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
         info+=&quot;Relation from <xsl:value-of select="$srcClass"/> (&quot;+String.valueOf(getSrc<xsl:value-of select="$srcClass"/>().getPrimaryKey())+&quot;)&quot;;
      </xsl:for-each><xsl:for-each select="toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        if(getDest<xsl:value-of select="$destClass"/>()!=null)
         info+=&quot;         to   <xsl:value-of select="$destClass"/> (&quot;+String.valueOf(getDest<xsl:value-of select="$destClass"/>().getPrimaryKey())+&quot;)&quot;;
      </xsl:for-each></xsl:if>
    return info;
    }

    public void setEntityContext(EntityContext ctx)
    {
      this.ctx=ctx;
    }
    
    public void unsetEntityContext()
    {
      this.ctx=null;
    }

    <xsl:if test="local-name()='objectClass'">
      /**
      * @ejb:create-method
      */
      public Integer ejbCreate(Integer objectId, VersionLocal version) throws CreateException
      {
        if(objectId == null)
        throw new CreateException(&quot;objectId is null :-o&quot;);
        this.setId(objectId);
        return null;
      }
      
      public void ejbPostCreate(Integer objectId, VersionLocal version)
      {
        this.setVersion(version);
      
      // to solve problems with untouched (null) boleans in the GUI:
      // initialize java.lang.Booleans inside SimpleProperties:
      <xsl:for-each select="simpleProperty"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable><xsl:if test="type='java.lang.Boolean'">
          set<xsl:value-of select="$propName"/>(new java.lang.Boolean(false));
        </xsl:if></xsl:for-each>

      // initialize VectorSets
        <xsl:for-each select="vectorSet"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>          
          set<xsl:value-of select="$propName"/>(new <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/>());
        </xsl:for-each>
        }
    </xsl:if><xsl:if test="local-name()='relationClass'">
      /**
      * @ejb:create-method
      */
      public Integer ejbCreate(Integer objectId, VersionLocal version,Object src,Object dest) throws CreateException
      {
        if(objectId == null)
        throw new CreateException(&quot;objectId is null :-o&quot;);
        this.setId(objectId);
        return null;
      }
      
      public void ejbPostCreate(Integer objectId, VersionLocal version,Object src,Object dest)
      {
      // set all relations to null:
      <xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        this.setSrc<xsl:value-of select="$srcClass"/>(null);
      </xsl:for-each>
      <xsl:for-each select="../toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
        this.setDest<xsl:value-of select="$destClass"/>(null);          
      </xsl:for-each>

      <xsl:for-each select="fromObjectClass"><xsl:variable name="srcClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable><xsl:for-each select="../toObjectClass"><xsl:variable name="destClass"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@ref"/></xsl:call-template></xsl:variable>
          if((src instanceof <xsl:value-of select="$srcClass"/>Local)
          <![CDATA[ && ]]>
            (dest instanceof <xsl:value-of select="$destClass"/>Local))
          {
            this.setVersion(version);
            this.setSrc<xsl:value-of select="$srcClass"/>((<xsl:value-of select="$srcClass"/>Local)src);
            this.setDest<xsl:value-of select="$destClass"/>((<xsl:value-of select="$destClass"/>Local)dest);          
          }                    
        </xsl:for-each></xsl:for-each>

      // initialize VectorSets
        <xsl:for-each select="vectorSet"><xsl:variable name="propName"><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template></xsl:variable>          
          set<xsl:value-of select="$propName"/>(new <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/>());
        </xsl:for-each>

      }
    </xsl:if>    
    
    public void ejbActivate()
    {}

    public void ejbPassivate()
    {}
    
    public void ejbLoad()
    {}

    public void ejbStore()
    {}
    
    public void ejbRemove() throws javax.ejb.RemoveException
    {
    }
    }    

  </xsl:template>
  <xsl:template name="toUpperCase">
    <xsl:param name="word"/>
    <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
    <xsl:value-of select="substring($word,2)"/>
  </xsl:template>
  <xsl:template name="getType">
    <xsl:param name="type"/>
    <xsl:choose>
      <xsl:when test="$type='id'">int</xsl:when>
      <xsl:when test="$type='integer'">int</xsl:when>
      <xsl:when test="$type='string'">String</xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="type"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
