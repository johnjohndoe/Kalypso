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

    package <xsl:value-of select="$package"/>;

/**
 * java-code for BEAN:
 *
 * Id-Manager
 *
 */

import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;  


/**
 * 
 * @ejb:bean   name="<xsl:value-of select="$themeName"/>.IdManager"
 *             jndi-name="ejb/<xsl:value-of select="$themeName"/>.IdManager"
 *             local-jndi-name="ejb/<xsl:value-of select="$themeName"/>.IdManager"
 *             view-type="local"
 *             type="CMP"
 *             cmp-version="2.x"
 *             primkey-field="tableName"
 * @ejb:pk generate="false" class="java.lang.String"
 * @jboss:create-table create="true"
    <!-- * @jboss:remove-table remove="true" -->
 */
public abstract class IdManagerBean implements EntityBean
{
    private transient EntityContext ctx;
    
    /**
     * @ejb:persistent-field
     */      
    public abstract void setTableName(java.lang.String tableName);

    /**
     * @ejb:persistent-field
     */
    public abstract java.lang.String getTableName();
    
    // ejb:pk-field
    
    /**
     * @ejb:persistent-field
     */
    public abstract void setMaxId(Integer maxId);
    
    /**
     * @ejb:persistent-field
     */
    public abstract Integer getMaxId();
    
    /**
     * @ejb:interface-method
     */
    public Integer useId()
    {
	Integer iD=getMaxId();
	setMaxId(new Integer(iD.intValue()+1));
	System.out.println("IdManagerBean: generated ID #"+iD.intValue()+" for table \""+getTableName()+"\"");
	return iD;
    }

    /**
     * @ejb:create-method
     */
    public String ejbCreate(String tableName) throws CreateException
    {
	if(tableName == null)  throw new CreateException("tableName is null :-(");
        this.setTableName(tableName);
        this.setMaxId(new Integer(1));
	return null;
    }
    
    public void ejbPostCreate(String tableName)
    {
	System.out.println("IdManager: created IdManagerEntity for \""+tableName+"\"-Table.");
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

    //    public void ejbRemove()
    //    {}    
}    

       </xsl:template>
       
       <xsl:template name="toUpperCase">
         <xsl:param name="word"/>
         <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
         <xsl:value-of select="substring($word,2)"/>
       </xsl:template>
       
     </xsl:stylesheet>
