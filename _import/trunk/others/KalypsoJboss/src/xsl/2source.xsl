<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text"/>
  <xsl:variable name="package" select="/theme/package"/>
  <xsl:variable name="themeName">
    <xsl:call-template name="toUpperCase">
      <xsl:with-param name="word" select="/theme/@ID"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="themeKey" select="/theme/@ID"/>

  <xsl:variable name="blank"> </xsl:variable>


  <xsl:template match="/">
    <xsl:apply-templates select="theme"/>
  </xsl:template>
  
  <xsl:template match="theme">
    
    <xsl:apply-templates mode="bean" select="child::*[self::objectClass or self::relationClass]"/>
    
    <xsl:apply-templates mode="session-bean" select="child::*[self::objectClass or self::relationClass]"/>
    
  </xsl:template>

  <xsl:template mode="session-bean" match="theme//child::*[self::objectClass or self::relationClass]">
    <xsl:variable name="objectClass">
      <xsl:call-template name="toUpperCase">
        <xsl:with-param name="word" select="@ID"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="relationClass">
      <xsl:value-of select="$objectClass"/>
    </xsl:variable>
    

  </xsl:template>





































         <!--===============================================================================-->
         <!--                                                                               -->
         <!--                             EntityBean                                        -->
         <!--                                                                               -->
         <!--===============================================================================-->

  <xsl:template mode="bean" match="theme//child::*[self::objectClass or self::relationClass]">
    <xsl:variable name="name">
      <xsl:call-template name="toUpperCase">
        <xsl:with-param name="word" select="@ID"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="objectClass">
      <xsl:call-template name="toUpperCase">
        <xsl:with-param name="word" select="@ID"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="relationClass">
      <xsl:value-of select="$objectClass"/>
    </xsl:variable>
    <xsl:variable name="elementTable">
      <xsl:value-of select="position()-1"/>
    </xsl:variable>
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
    import java.io.Writer;
    import java.io.IOException;

    import java.util.Vector;
    import java.util.ArrayList;
    import java.util.Collection;
    import java.util.Iterator;  

    import de.tuhh.wb.javagis.model.BasePointTransfer;
    import java.text.ParseException;
    import java.text.SimpleDateFormat;
    import java.text.DateFormat;
    import java.util.Date;

    import ejb.event.EJBEvent;

    import javax.jms.JMSException;
    import javax.naming.NamingException;   
    import ejb.event.EJBEventHelper;

    import de.tuhh.wb.javagis.xml.VectorSet;
    import de.tuhh.wb.javagis.xml.XmlConvert;
    import de.tuhh.wb.javagis.model.ElementLocal;
    import de.tuhh.wb.javagis.model.VersionLocal;
    import de.tuhh.wb.javagis.xml.GisTransferObject;
    import de.tuhh.wb.javagis.model.Tools;    
    <xsl:if test="//vectorSet">
      import <xsl:value-of select="$package"/>.vectorsets.*;         
    </xsl:if>
   
    <!--  
         * @ejb:finder signature="java.util.Collection findOrdered()"
         * @jboss:finder-query name="findOrdered" query="foo = {1}" order="foo,bar"
         * @ejb:finder signature="Collection findLargeAccounts(int balance)" role-name="Teller,IRS"
         * @jboss:finder-query name="findLargeAccounts" query="$1 = $1" order="balance" read-ahead="true"
         -->
    /**
    * @ejb:bean   name="<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>"
    *             jndi-name="ejb/<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>"
    *             local-jndi-name="<xsl:value-of select="$themeName"/>.<xsl:value-of select="$name"/>"    
    *             view-type="local"
    *             type="CMP"
    *             cmp-version="2.x"
    *             schema="<xsl:value-of select="$themeName"/>_<xsl:value-of select="$name"/>"
    *             primkey-field="id"
    * @jboss:create-table create="true"
    <!--    * @jboss:remove-table remove="true" -->
    * @ejb:pk generate="false" class="java.lang.Integer"
    * @ejb:ejb-ref ejb-name="<xsl:value-of select="$themeName"/>.Version" view-type="local"
    <xsl:if test="local-name()='objectClass'">
      * @ejb:interface local-extends="de.tuhh.wb.javagis.model.ObjectLocal"
    </xsl:if>
    <xsl:if test="local-name()='relationClass'">
      * @ejb:interface local-extends="de.tuhh.wb.javagis.model.RelationLocal"
    </xsl:if><!--
    --><xsl:for-each select="simpleProperty">
    <xsl:variable name="fieldName">
      <xsl:call-template name="toUpperCase">
        <xsl:with-param name="word" select="@key"/>
      </xsl:call-template>
    </xsl:variable>
    * @ejb:finder signature="java.util.Collection findBy<xsl:value-of select="$fieldName"/>(<xsl:value-of select="type"/><xsl:value-of select="' '"/><xsl:value-of select="@key"/>)" 
    * query="SELECT OBJECT(o) FROM <xsl:value-of select="$themeName"/>_<xsl:value-of select="$name"/> AS o WHERE o.<xsl:value-of select="@key"/> =?1"
    <!--
         * @ejb:finder signature="java.util.Collection findOrderedBy<xsl:value-of select="$fieldName"/>()"
         * @jboss:finder-query name="findOrderedBy<xsl:value-of select="$fieldName"/>" query="1-1" order="<xsl:value-of select="@key"/>" read-ahead="true"
         --></xsl:for-each>
    * @ejb:resource-ref res-name="ConnectionFactory"
    *                   res-type="javax.jms.TopicConnectionFactory"
    *                   res-auth="Container"
    */
    public abstract class <xsl:value-of select="$name"/>Bean implements EntityBean
    {
    private transient EntityContext ctx;
    
    // relations...
    /**
    * @ejb:interface-method
    * @ejb:relation name="Versioning-of-<xsl:value-of select="$name"/>"
    *               role-name="<xsl:value-of select="$name"/>-has-Version"<!--
--><xsl:if test="local-name()='objectClass'">
    *               cascade-delete="yes"<!-- version deletes objects deletes relations --><!--     
--></xsl:if>
    * @jboss:relation fk-constraint="false"
    *                 related-pk-field="id"
    *                 fk-column="version"
    */
    public abstract VersionLocal getVersion();
    <xsl:if test="local-name()='relationClass'">
      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name="<xsl:value-of select="$srcClass"/>_<xsl:value-of select="$relationClass"/>"
        *               role-name="<xsl:value-of select="$relationClass"/>_to_<xsl:value-of select="$srcClass"/>"
        *               cascade-delete="yes" <!-- version deletes objects deletes relations -->
        * @jboss:relation fk-constraint="false"
        *                 related-pk-field="id"
        *                 fk-column="relTo<xsl:value-of select="$srcClass"/>"
        */
        public abstract <xsl:value-of select="$srcClass"/>Local getSrc<xsl:value-of select="$srcClass"/>();

        /**
        * @ejb:interface-method
        */
        public abstract void setSrc<xsl:value-of select="$srcClass"/>(<xsl:value-of select="$srcClass"/>Local source);
      </xsl:for-each>

      <xsl:for-each select="toObjectClass">
        <xsl:variable name="destClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name="<xsl:value-of select="$relationClass"/>_<xsl:value-of select="$destClass"/>"
        *               role-name="<xsl:value-of select="$relationClass"/>_to_<xsl:value-of select="$destClass"/>"
        *               cascade-delete="yes" <!-- version deletes objects deletes relations -->
        * @jboss:relation fk-constraint="false"
        *                 related-pk-field="id"
        *                 fk-column="relFrom<xsl:value-of select="$destClass"/>"
        */
        public abstract <xsl:value-of select="$destClass"/>Local getDest<xsl:value-of select="$destClass"/>();

        /**
        * @ejb:interface-method
        */
        public abstract void setDest<xsl:value-of select="$destClass"/>(<xsl:value-of select="$destClass"/>Local destination);
      </xsl:for-each>
    </xsl:if>

    <xsl:if test="local-name()='objectClass'">
      <xsl:variable name="objectKey" select="@ID"/>
      <xsl:for-each select="//fromObjectClass[@ref=$objectKey]">
        <xsl:variable name="relationClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="../@ID"/>
          </xsl:call-template>
        </xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name="<xsl:value-of select="$objectClass"/>_<xsl:value-of select="$relationClass"/>"
        *               role-name="<xsl:value-of select="$objectClass"/>_to_<xsl:value-of select="$relationClass"/>"
        */
        public abstract Collection getRelTo<xsl:value-of select="$relationClass"/>();        

        /**
        * @ejb:interface-method
        */
        public abstract void setRelTo<xsl:value-of select="$relationClass"/>(Collection col);        
      </xsl:for-each>      

      <xsl:if test="local-name()='objectClass'">
        <xsl:variable name="objectKey" select="@ID"/>
        /**
        * @ejb:interface-method
        */
        public Vector returnForwardRelations()
        {
         Vector result=new Vector();
         Collection collection;

         <xsl:for-each select="//fromObjectClass[@ref=$objectKey]">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="../@ID"/>
             </xsl:call-template>
           </xsl:variable>
           collection=getRelTo<xsl:value-of select="$relationClass"/>();
           if(collection.size()>0)
           {
           Vector relation=new Vector();
           Vector idList=new Vector();
           Iterator it=collection.iterator();
           while(it.hasNext())
           {
           idList.add(((ElementLocal)it.next()).getId());
           }                     
           relation.add("<xsl:value-of select="../@ID"/>"); // key
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

         <xsl:for-each select="//toObjectClass[@ref=$objectKey]">
           <xsl:variable name="relationClass">
             <xsl:call-template name="toUpperCase">
               <xsl:with-param name="word" select="../@ID"/>
             </xsl:call-template>
           </xsl:variable>
           collection=getRelFrom<xsl:value-of select="$relationClass"/>();
           if(collection.size()>0)
           {
            Vector relation=new Vector();
            Vector idList=new Vector();
            Iterator it=collection.iterator();
            while(it.hasNext())
            {
             idList.add(((ElementLocal)it.next()).getId());
            }                     
           relation.add("<xsl:value-of select="../@ID"/>"); // key
            relation.add(idList); // IDs
            result.add(relation);
           }
         </xsl:for-each>      
         return result;
        }
      </xsl:if>


      <xsl:for-each select="//toObjectClass[@ref=$objectKey]">
        <xsl:variable name="relationClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="../@ID"/>
          </xsl:call-template>
        </xsl:variable>
        /**
        * @ejb:interface-method
        * @ejb:relation name="<xsl:value-of select="$relationClass"/>_<xsl:value-of select="$objectClass"/>"
        *               role-name="<xsl:value-of select="$objectClass"/>_to_<xsl:value-of select="$relationClass"/>"
        */
        public abstract Collection getRelFrom<xsl:value-of select="$relationClass"/>();        

        /**
        * @ejb:interface-method
        */
        public abstract void setRelFrom<xsl:value-of select="$relationClass"/>(Collection col);        
      </xsl:for-each>      
    </xsl:if>
      
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
      * @ejb:persistent-field
      */
      public abstract void setBasePointX(Double x);
    
      /**
      * @ejb:persistent-field
      */
      public abstract Double getBasePointX();

      /**
      * @ejb:persistent-field
      */
      public abstract void setBasePointY(Double y);
    
      /**
      * @ejb:persistent-field
      */
      public abstract Double getBasePointY();
    </xsl:if>
    
    /**
    * @ejb:interface-method
    */
    public BasePointTransfer getBasePoint()
    {
      <xsl:choose>
        <xsl:when test="symbol">
          Double x=getBasePointX();
          Double y=getBasePointY();
          if(x!=null <![CDATA[ && ]]> y !=null)
          {
          //           System.out.println("getBasePoint() got Basepoint");
           return new BasePointTransfer(x.doubleValue(),y.doubleValue());
          }
          else
          {
          //           System.out.println("getBasePoint() was null");
           return new BasePointTransfer(0d,0d);
          }
        </xsl:when>
        <xsl:otherwise>
          //          System.out.println("getBasePoint() Element has no symbol :-(");
          return null;
        </xsl:otherwise>
      </xsl:choose>
      }

      /**
      * @ejb:interface-method
      */
      public void setBasePoint(BasePointTransfer pos)
      {
      <xsl:if test="symbol">
        setBasePointX(new Double(pos.getX()));
        setBasePointY(new Double(pos.getY()));
        //EVENT:
         try
         {
        //           System.out.println("fire BASEPOINT_CHANGE");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.OBJECT_BASEPOINT_CHANGE,getVersion().getPrimaryKey(),<xsl:value-of select="$elementTable"/>,getId());
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
      //           System.out.println("fired BASEPOINT_CHANGE");
         }
         catch(NamingException e)
         {
           e.printStackTrace();
         }
         catch(JMSException e)
         {
           e.printStackTrace();
         }
      </xsl:if>
      }

    // simpleProperty..
    <xsl:for-each select="simpleProperty">
      <xsl:variable name="fieldName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="fieldType">
        <xsl:value-of select="type"/>          
      </xsl:variable>
      
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




    /**
    * @ejb:interface-method
    */
    public Vector getSimplePropertyRow()
    {
     Vector resultRow=new Vector();
     <xsl:for-each select="simpleProperty">
       <xsl:variable name="fieldName">
         <xsl:call-template name="toUpperCase">
           <xsl:with-param name="word" select="@key"/>
         </xsl:call-template>
       </xsl:variable>
       resultRow.add(get<xsl:value-of select="$fieldName"/>());
     </xsl:for-each>
     return resultRow;
    }

    /**
    * @ejb:interface-method
    */
    public Object getSimpleProperty(int pos)
    {
     switch(pos)
     {
     <xsl:for-each select="simpleProperty">
       <xsl:variable name="fieldName">
         <xsl:call-template name="toUpperCase">
           <xsl:with-param name="word" select="@key"/>
         </xsl:call-template>
       </xsl:variable>
       case <xsl:value-of select="position()-1"/>:
        return get<xsl:value-of select="$fieldName"/>();
     </xsl:for-each>
       default:
        break;
     }
     return null;
    }


    /**
    * @ejb:interface-method
    */
    public void setSimpleProperty(int pos,Object value)
    {
     switch(pos)
     {
     <xsl:for-each select="simpleProperty">
       <xsl:variable name="fieldName">
         <xsl:call-template name="toUpperCase">
           <xsl:with-param name="word" select="@key"/>
         </xsl:call-template>
       </xsl:variable>
       case <xsl:value-of select="position()-1"/>:
        set<xsl:value-of select="$fieldName"/>((<xsl:value-of select="type"/>)value);
       break;
     </xsl:for-each>
       default:
        break;
     }
    }
        
    // vectorSet...
     /**
      * @ejb:interface-method
     */
     public void setVectorSet(int pos,Object value)
     {
      switch(pos)
       {      
       <xsl:for-each select="vectorSet">
         <xsl:variable name="vectorSetName">
           <xsl:call-template name="toUpperCase">
             <xsl:with-param name="word" select="@key"/>
           </xsl:call-template>
         </xsl:variable>
         case <xsl:value-of select="position()-1"/>:
          set<xsl:value-of select="$vectorSetName"/>((<xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/>) value);
          break;
       </xsl:for-each>
       default:
       break;
     }
     }

     /**
      * @ejb:interface-method
     */
     public Object getVectorSet(int pos)
     {
      switch(pos)
       {      
       <xsl:for-each select="vectorSet">
         <xsl:variable name="vectorSetName">
           <xsl:call-template name="toUpperCase">
             <xsl:with-param name="word" select="@key"/>
           </xsl:call-template>
         </xsl:variable>
         case <xsl:value-of select="position()-1"/>:
          return get<xsl:value-of select="$vectorSetName"/>();
       </xsl:for-each>
       default:
       break;
     }
       return null;
     }

       /**
       * @ejb:interface-method
       */
     public Vector getVectorSets()
     {
       Vector result=new Vector();
       <xsl:for-each select="vectorSet">
         <xsl:variable name="vectorSetName">
           <xsl:call-template name="toUpperCase">
             <xsl:with-param name="word" select="@key"/>
           </xsl:call-template>
         </xsl:variable>      
         result.add(get<xsl:value-of select="$vectorSetName"/>());
       </xsl:for-each>
       return result;
     }

       /**
       * @ejb:interface-method
       */
     public void setVectorSets(Vector vectorSets)
     {
       <xsl:for-each select="vectorSet">
         <xsl:variable name="vectorSetName">
           <xsl:call-template name="toUpperCase">
             <xsl:with-param name="word" select="@key"/>
           </xsl:call-template>
         </xsl:variable>      
         set<xsl:value-of select="$vectorSetName"/>((<xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/>) vectorSets.elementAt(<xsl:value-of select="position()-1"/>));
       </xsl:for-each>
     }

    <xsl:for-each select="vectorSet">
      <xsl:variable name="vectorSetName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>      
      // VectorSet-Set Dummy
      /**
      * @ejb:interface-method
      */
      public void set<xsl:value-of select="$vectorSetName"/>(<xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> value)
      {
       set<xsl:value-of select="$vectorSetName"/>(value,true);
      }

      private void set<xsl:value-of select="$vectorSetName"/>(<xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> value,boolean withEvent)
      {
       VectorSet vs=value.toVectorSetTransferObject();
       String xml=vs.toXmlString();
      //       System.out.println("setVectorSet: "+xml);
       setVec<xsl:value-of select="$vectorSetName"/>(xml);
        //EVENT:
        if(withEvent)
        {
         try
         {
      //           System.out.println("fire VectorSetEvent");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.VECTORSET_CHANGE,getVersion().getPrimaryKey(),<xsl:value-of select="$elementTable"/>,getId());
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
      //           System.out.println("fired VectorSetEvent");
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

      
      /**
      * @ejb:persistent-field
      */
      public abstract void setVec<xsl:value-of select="$vectorSetName"/>(String value);
      

      //VectorSetgetDummy:
      /**
      * @ejb:interface-method
      */
      public <xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> get<xsl:value-of select="$vectorSetName"/>()
      {
       String xml=getVec<xsl:value-of select="$vectorSetName"/>();
       <xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/> vectorSet=
          new <xsl:value-of select="$objectClass"/><xsl:value-of select="$vectorSetName"/>();
       try
       {
        XmlConvert xmlConvert=new XmlConvert();
        xmlConvert.parse(xml);
        VectorSet vsto=xmlConvert.toVectorSet();
        vectorSet.loadFromVectorSetTransferObject(vsto);
       }	
       catch(Exception e)
       {
        e.printStackTrace();
       }
       return vectorSet;
      }
      
      /**
      * @ejb:persistent-field
      */
      public abstract String getVec<xsl:value-of select="$vectorSetName"/>();
      

      

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
    public GisTransferObject toGisTransferObject()
    {
      String tableName="<xsl:value-of select="@ID"/>";
      GisTransferObject gto=new GisTransferObject(tableName,String.valueOf(getId()),true);
    
 

    <xsl:if test="symbol">
      String[] bpTags={"x","y"};
      Double dbpx=getBasePointX();
      Double dbpy=getBasePointY();
      if(dbpx!=null  <![CDATA[ && ]]> dbpy!=null)
      {
       gto.addBasePoint("x",dbpx.toString());
       gto.addBasePoint("y",dbpy.toString());
      //       AttributesImpl basePointAtt=new AttributesImpl();
      //       basePointAtt.addAttribute();
      }
      else
      {
       gto.addBasePoint("x","null"); // Todo; weglassen ?
       gto.addBasePoint("y","null");
      }
    </xsl:if>

    String propKey=null;
    Object propValue=null;

    <xsl:for-each select="simpleProperty">
      <xsl:variable name="Property">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      propKey="<xsl:value-of select="@key"/>";
      <xsl:choose>
        <xsl:when test="type='java.util.Date'">
          propValue=date2Millis(get<xsl:value-of select="$Property"/>());
        </xsl:when>
        <xsl:otherwise>
          propValue=get<xsl:value-of select="$Property"/>();
        </xsl:otherwise>
      </xsl:choose>
      if(propValue!=null)
       gto.addSimpleProperty( propKey, propValue.toString());
      else
       gto.addSimpleProperty( propKey, "null"); //Todo: weglassen
    </xsl:for-each>
    
      VectorSet vto;    
    // vectorSetsToXML:
    <xsl:for-each select="vectorSet">
      <xsl:variable name="vectorSetName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      vto=get<xsl:value-of select="$vectorSetName"/>().toVectorSetTransferObject();
      gto.addVectorSet(vto);
    </xsl:for-each>

    <xsl:if test="local-name()='relationClass'">

      Object srcId =null;
      String srcKey=null;
      Object destId =null;
      String destKey=null;

      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
        {
         srcId=getSrc<xsl:value-of select="$srcClass"/>().getId();
         srcKey="<xsl:value-of select="@ref"/>";
        }
      </xsl:for-each>
      <xsl:for-each select="toObjectClass">
        <xsl:variable name="destClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        
        if(getDest<xsl:value-of select="$destClass"/>()!=null)
        {
         destId=getDest<xsl:value-of select="$destClass"/>().getId();              
         destKey="<xsl:value-of select="@ref"/>";
        }
      </xsl:for-each>
      gto.setRelationSrcTable(srcKey);
      gto.setRelationDestTable(destKey);
      gto.setRelationSrcIdentifier(srcId.toString());
      gto.setRelationDestIdentifier(destId.toString());
    </xsl:if>
     return gto;
    }

















    /**
    * @ejb:interface-method
    */
    public void toXML(Writer out) throws IOException
    {
     Tools.genXmlTag(out,"o","ID",String.valueOf(getId()));

    <xsl:if test="symbol">
      String[] bpTags={"x","y"};
      Double dbpx=getBasePointX();
      Double dbpy=getBasePointY();
      if(dbpx!=null  <![CDATA[ && ]]> dbpy!=null)
      {
       String[] bpValues={dbpx.toString(),dbpy.toString()};
       Tools.genXml(out,"bp",bpTags,bpValues,"");
      }
      else
      {
       String[] bpValues={"null","null"};
       Tools.genXml(out,"bp",bpTags,bpValues,"");
      }
    </xsl:if>

    Object[] simpleProps=
    {
    <xsl:for-each select="simpleProperty">
      <xsl:variable name="Property">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:choose>
        <xsl:when test="type='java.util.Date'">
          date2Millis(get<xsl:value-of select="$Property"/>())
        </xsl:when>
        <xsl:otherwise>
          get<xsl:value-of select="$Property"/>()
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="position() != last()">,</xsl:if>
    </xsl:for-each>};
    
    //    Tools.genXmlOpenTag(out,"sp",<xsl:value-of select="$objectClass"/>SessionBean.simplePropertyKeys,simpleProps);
    
    // vectorSetsToXML:
    <xsl:for-each select="vectorSet">
      <xsl:variable name="vectorSetName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      get<xsl:value-of select="$vectorSetName"/>().toXml(out);
    </xsl:for-each>
    Tools.genXmlTag(out,"/sp");
    <xsl:if test="local-name()='relationClass'">

      Object srcId =null;
      String srcKey=null;
      Object destId =null;
      String destKey=null;

      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
        {
         srcId=getSrc<xsl:value-of select="$srcClass"/>().getId();
         srcKey="<xsl:value-of select="@ref"/>";
        }
      </xsl:for-each>
      <xsl:for-each select="toObjectClass">
        <xsl:variable name="destClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        
        if(getDest<xsl:value-of select="$destClass"/>()!=null)
        {
         destId=getDest<xsl:value-of select="$destClass"/>().getId();              
         destKey="<xsl:value-of select="@ref"/>";
        }
      </xsl:for-each>

      String[] attNames={"srcKey","srcID","destKey","destID"};
      String[] atts={srcKey,srcId.toString(),destKey,destId.toString()};
      Tools.genXml(out,"rel",attNames,atts,"");      
    </xsl:if>
    
     Tools.genXmlTag(out,"/o");
    }

    /**
    * @ejb:interface-method
    */
    public void loadGisTransferObject(GisTransferObject transferObject)
    {
    <xsl:if test="symbol">
      // basePoints...
      String pointX=transferObject.getBasePointProperty("x");
      String pointY=transferObject.getBasePointProperty("y");
      //      System.out.println("readGisPoint...");
      if(pointX!=null  <![CDATA[ && ]]> !"null".equals(pointX) <![CDATA[ && ]]>
         pointY!=null  <![CDATA[ && ]]> !"null".equals(pointY))
      {
       setBasePointX(new Double(pointX));
       setBasePointY(new Double(pointY));
      //       System.out.println("got GisPoint "+pointX+" "+pointY);
      }
    </xsl:if>
    

    // SimpleProperties:
    String value;
    java.util.GregorianCalendar calendar=new java.util.GregorianCalendar();
    <xsl:for-each select="simpleProperty">
      <xsl:variable name="Property">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>
      value=transferObject.getSimpleProperty("<xsl:value-of select="@key"/>");
      if(value!=null <![CDATA[ && ]]> !"null".equals(value))
      {
      <xsl:choose>
        <xsl:when test="type='java.util.Date'">
          try
          {
           calendar.setTimeInMillis(java.lang.Long.parseLong(value));
           set<xsl:value-of select="$Property"/>(calendar.getTime());
          }
          catch(Exception e)
          {
           System.out.println("problem parsing date (not in longformat?)");
           System.out.println(e.getMessage());
          }
       </xsl:when>
       <xsl:otherwise>
         set<xsl:value-of select="$Property"/>(new <xsl:value-of select="type"/>(value));
       </xsl:otherwise>
      </xsl:choose>
      }
    </xsl:for-each>
    
    // VectorSets:
    <xsl:for-each select="vectorSet">
      <xsl:variable name="propName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@key"/>
        </xsl:call-template>
      </xsl:variable>          
      // get vectorSet
      <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/> my<xsl:value-of select="$propName"/> = get<xsl:value-of select="$propName"/>();
      // set vectorSet
      my<xsl:value-of select="$propName"/>.loadFromGisTransferObject(transferObject);
      set<xsl:value-of select="$propName"/>(my<xsl:value-of select="$propName"/>,false);
    </xsl:for-each>
    }

    /**
    * @ejb:interface-method
    */
    public String getInfo()
    {
      String info="ID#"+String.valueOf(getId());

    <xsl:if test="local-name()='objectClass'">
      <xsl:variable name="objectKey" select="@ID"/>
      <xsl:for-each select="//fromObjectClass[@ref=$objectKey]">
        <xsl:variable name="relationClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="../@ID"/>
          </xsl:call-template>
        </xsl:variable>
        info+=" has "+String.valueOf(getRelTo<xsl:value-of select="$relationClass"/>().size())+" Relations to <xsl:value-of select="$relationClass"/>";
      </xsl:for-each>      

      <xsl:for-each select="//toObjectClass[@ref=$objectKey]">
        <xsl:variable name="relationClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="../@ID"/>
          </xsl:call-template>
        </xsl:variable>
        info+=" has "+String.valueOf(getRelFrom<xsl:value-of select="$relationClass"/>().size())+" Relations from <xsl:value-of select="$relationClass"/>";
      </xsl:for-each>      
    </xsl:if>

    <xsl:if test="local-name()='relationClass'">
      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
         info+="Relation from <xsl:value-of select="$srcClass"/> ("+String.valueOf(getSrc<xsl:value-of select="$srcClass"/>().getPrimaryKey())+")";
      </xsl:for-each>

      <xsl:for-each select="toObjectClass">
        <xsl:variable name="destClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        if(getDest<xsl:value-of select="$destClass"/>()!=null)
         info+="         to   <xsl:value-of select="$destClass"/> ("+String.valueOf(getDest<xsl:value-of select="$destClass"/>().getPrimaryKey())+")";
      </xsl:for-each>
    </xsl:if>
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
      public Integer ejbCreate(Integer oId, VersionLocal version,boolean withEvent) throws CreateException
      {
        if(oId == null)
        throw new CreateException("objectId is null :-o");
        this.setId(oId);
        return null;
      }
      
      public void ejbPostCreate(Integer objectId, VersionLocal version,boolean withEvent)
      {
        this.setVersion(version);
      
      // to solve problems with untouched (null) boleans in the GUI:
      // initialize java.lang.Booleans inside SimpleProperties:
      <xsl:for-each select="simpleProperty">
        <xsl:variable name="propName">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@key"/>
          </xsl:call-template>
        </xsl:variable>          

        <xsl:if test="type='java.lang.Boolean'">
          set<xsl:value-of select="$propName"/>(new java.lang.Boolean(false));
        </xsl:if>
        <xsl:if test="default">
          set<xsl:value-of select="$propName"/>(new <xsl:value-of select="type"/>(<xsl:value-of select="default"/>));
        </xsl:if>
      </xsl:for-each>

      // initialize VectorSets
        <xsl:for-each select="vectorSet">
          <xsl:variable name="propName">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@key"/>
            </xsl:call-template>
          </xsl:variable>          
          set<xsl:value-of select="$propName"/>(new <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/>(),false);
        </xsl:for-each>
        //EVENT:
        if(withEvent)
        {
         try
         {
        //           System.out.println("fire ObjectEvent");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.OBJECT_CREATE,version.getPrimaryKey(),<xsl:value-of select="position()-1"/>,objectId);
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
        //           System.out.println("fired ObjectEvent");
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
    </xsl:if>
    
    <xsl:if test="local-name()='relationClass'">
      <xsl:variable name="relationClass">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="@ID"/>
        </xsl:call-template>
      </xsl:variable>
      /**
      * @ejb:create-method
      */
      public Integer ejbCreate(Integer objectId, VersionLocal version,Object src,Object dest,boolean withEvent) throws CreateException
      {
        if(objectId == null)
         throw new CreateException("objectId is null :-o");

      //Check for cardinality
        <xsl:variable name="srcCard"  select="fromCardinalityMax"/>
        <xsl:variable name="destCard" select="toCardinalityMax"/>
        Collection col=null;



        <xsl:if test="$srcCard != 'unbound'">
          <xsl:for-each select="toObjectClass">
            <xsl:variable name="destClass">
              <xsl:call-template name="toUpperCase">
                <xsl:with-param name="word" select="@ref"/>
              </xsl:call-template>
            </xsl:variable>
            if(dest instanceof <xsl:value-of select="$destClass"/>Local)
            {
             col=((<xsl:value-of select="$destClass"/>Local)dest).getRelFrom<xsl:value-of select="$relationClass"/>();        
             if(col.size() <![CDATA[ >= ]]> <xsl:value-of select="$srcCard"/>)
               throw new CreateException("max Cardinality is allready reached");
            }
          </xsl:for-each>
        </xsl:if>

        <xsl:if test="$destCard != 'unbound'">
          <xsl:for-each select="fromObjectClass">
            <xsl:variable name="srcClass">
              <xsl:call-template name="toUpperCase">
                <xsl:with-param name="word" select="@ref"/>
              </xsl:call-template>
            </xsl:variable>
            if(src instanceof <xsl:value-of select="$srcClass"/>Local)
            {
             col=((<xsl:value-of select="$srcClass"/>Local)src).getRelTo<xsl:value-of select="$relationClass"/>();        
             if(col.size() <![CDATA[ >= ]]> <xsl:value-of select="$destCard"/>)
              throw new CreateException("max Cardinality is allready reached");
            }
          </xsl:for-each>
        </xsl:if>

        this.setId(objectId);
        return null;
      }
      
      public void ejbPostCreate(Integer objectId, VersionLocal version,Object src,Object dest,boolean withEvent)
      {
      // set all relations to null:
      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        this.setSrc<xsl:value-of select="$srcClass"/>(null);
      </xsl:for-each>
      <xsl:for-each select="../toObjectClass">
        <xsl:variable name="destClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        this.setDest<xsl:value-of select="$destClass"/>(null);          
      </xsl:for-each>

      <xsl:for-each select="fromObjectClass">
        <xsl:variable name="srcClass">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="@ref"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:for-each select="../toObjectClass">
          <xsl:variable name="destClass">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@ref"/>
            </xsl:call-template>
          </xsl:variable>
          if((src instanceof <xsl:value-of select="$srcClass"/>Local)
          <![CDATA[ && ]]>
            (dest instanceof <xsl:value-of select="$destClass"/>Local))
          {
            this.setVersion(version);
            this.setSrc<xsl:value-of select="$srcClass"/>((<xsl:value-of select="$srcClass"/>Local)src);
            this.setDest<xsl:value-of select="$destClass"/>((<xsl:value-of select="$destClass"/>Local)dest);
          }                    
        </xsl:for-each>
      </xsl:for-each>

      // initialize VectorSets
        <xsl:for-each select="vectorSet">
          <xsl:variable name="propName">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@key"/>
            </xsl:call-template>
          </xsl:variable>          
          set<xsl:value-of select="$propName"/>(new <xsl:value-of select="$objectClass"/><xsl:value-of select="$propName"/>(),false);
        </xsl:for-each>
        //EVENT:
        if(withEvent)
        {
         try
         {
        //           System.out.println("fire RelationEvent");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.RELATION_CREATE,version.getPrimaryKey(),<xsl:value-of select="position()-1"/>,objectId);
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
        //           System.out.println("fired RelationEvent");
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

        /**
        * interface-method
        */
        public Object getSrcId()
        {
        <xsl:for-each select="fromObjectClass">
          <xsl:variable name="srcClass">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@ref"/>
            </xsl:call-template>
          </xsl:variable>
          if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
           return getSrc<xsl:value-of select="$srcClass"/>().getId();
        </xsl:for-each>
        return null;
        }

        /**
        * interface-method
        */
        public Object getDestId()
        {
        <xsl:for-each select="toObjectClass">
          <xsl:variable name="destClass">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@ref"/>
            </xsl:call-template>
          </xsl:variable>
          if(getDest<xsl:value-of select="$destClass"/>()!=null)
           return getDest<xsl:value-of select="$destClass"/>().getId();
        </xsl:for-each>
        return null;
        }

        /**
        * interface-method
        */
        public String getSrcKey()
        {
        <xsl:for-each select="fromObjectClass">
          <xsl:variable name="srcClass">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@ref"/>
            </xsl:call-template>
          </xsl:variable>
          if(getSrc<xsl:value-of select="$srcClass"/>()!=null)
           return "<xsl:value-of select="@ref"/>";
        </xsl:for-each>
        return null;
        }

        /**
        * interface-method
        */
        public String getDestKey()
        {
        <xsl:for-each select="toObjectClass">
          <xsl:variable name="destClass">
            <xsl:call-template name="toUpperCase">
              <xsl:with-param name="word" select="@ref"/>
            </xsl:call-template>
          </xsl:variable>
          if(getDest<xsl:value-of select="$destClass"/>()!=null)
           return "<xsl:value-of select="@ref"/>";
        </xsl:for-each>
        return null;
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
     VersionLocal myVersion=getVersion();
     if(myVersion!=null) // no version-delete
     {
       <xsl:if test="local-name()='objectClass'">
        //EVENT:
         try
         {
         //           System.out.println("fire remove ObjectEvent");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.OBJECT_REMOVE,getVersion().getPrimaryKey(),<xsl:value-of select="position()-1"/>,getId());
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
         //           System.out.println("fired remove ObjectEvent");
         }
         catch(NamingException e)
         {
           e.printStackTrace();
         }
         catch(JMSException e)
         {
           e.printStackTrace();
         }
    
       </xsl:if>
       <xsl:if test="local-name()='relationClass'">
        //EVENT:
         try
         {
         //           System.out.println("fire remove RelationEvent");
           EJBEvent event=new EJBEvent("<xsl:value-of select="$themeKey"/>",EJBEvent.RELATION_REMOVE,getVersion().getPrimaryKey(),<xsl:value-of select="position()-1"/>,getId());
           EJBEventHelper eventHelper=new EJBEventHelper();          
           eventHelper.fireEvent(event);
         //           System.out.println("fired remove RelationEvent");
         }
         catch(NamingException e)
         {
           e.printStackTrace();
         }
         catch(JMSException e)
         {
           e.printStackTrace();
         }
         </xsl:if>
         }
        }    
       }
  </xsl:template>
  





  
  <xsl:template name="toUpperCase">
    <xsl:param name="word"/>
    <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
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
