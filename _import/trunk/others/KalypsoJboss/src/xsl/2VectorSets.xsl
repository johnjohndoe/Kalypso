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
    <xsl:for-each select="/theme/child::*[self::objectClass or self::relationClass]">
      <xsl:variable name="myObjectClass" select="@ID"/>
      <xsl:variable name="MyObjectClass">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="$myObjectClass"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="MyObjectClassName">
        <xsl:call-template name="toUpperCase">
          <xsl:with-param name="word" select="name"/>
        </xsl:call-template>
      </xsl:variable>

      <xsl:for-each select=".//vectorSet">
        <xsl:variable name="myVectorSet" select="@key"/>
        <xsl:variable name="MyVectorSet">
          <xsl:call-template name="toUpperCase">
            <xsl:with-param name="word" select="$myVectorSet"/>
          </xsl:call-template>
        </xsl:variable>
        
        package <xsl:value-of select="$package"/>.vectorsets;

        import java.lang.*;
        import java.io.IOException;
        import java.io.Writer;
        import java.io.PrintWriter;
        import java.util.Vector;
        import de.tuhh.wb.javagis.model.Tools;    
        import de.tuhh.wb.javagis.xml.GisTransferObject;    
        import de.tuhh.wb.javagis.xml.VectorSet;    
        import java.text.ParseException;
        import java.text.SimpleDateFormat;
        import java.text.DateFormat;
        import java.util.Date;

        <xsl:choose>
          <xsl:when test="local-name(..)='vectorSet'">
            <xsl:variable name="Agregated">
              <xsl:value-of select="$MyObjectClass"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@key"/></xsl:call-template><xsl:value-of select="$MyVectorSet"/>
            </xsl:variable>
            //agregated Class
            public class <xsl:value-of select="$Agregated"/> 
          </xsl:when>
          <xsl:otherwise>
            public class <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/>
          </xsl:otherwise>
        </xsl:choose>
        implements de.tuhh.wb.javagis.model.GisInterfaceTableModel,
        de.tuhh.wb.javagis.model.GisToXmlConvertable,
        java.io.Serializable
        {
          public final static String[] simplePropertyKeys={
          <xsl:for-each select="simpleProperty">
           "<xsl:value-of select="@key"/>"<xsl:if test="position() != last()">,</xsl:if>
          </xsl:for-each>};
          public final static String[] simplePropertyNames={
          <xsl:for-each select="simpleProperty">
            "<xsl:value-of select="name"/> [<xsl:value-of select="unit"/>]"<xsl:if test="position() != last()">,</xsl:if>
          </xsl:for-each>};
          public final static String[] simplePropertyDescriptions={
          <xsl:for-each select="simpleProperty">
           "<xsl:value-of select="description"/>"<xsl:if test="position() != last()">,</xsl:if>
          </xsl:for-each>};
          public final static String[] simplePropertyUnits={
          <xsl:for-each select="simpleProperty">
           "<xsl:value-of select="unit"/>"<xsl:if test="position() != last()">,</xsl:if>
          </xsl:for-each>};
          public final static Class[] simplePropertyClasses={
          <xsl:for-each select="simpleProperty">
           <xsl:value-of select="type"/>.class<xsl:if test="position() != last()">,</xsl:if>
          </xsl:for-each>};                     

           public final static String[] simplePropertyFormats={
           <xsl:for-each select="simpleProperty">
             <xsl:choose>
               <xsl:when test="format">
                 "<xsl:value-of select="format"/>"                            
               </xsl:when>
               <xsl:otherwise>
                 "d.M.y H:m:s"
               </xsl:otherwise>
             </xsl:choose>
             <xsl:if test="position() != last()">,</xsl:if>             
           </xsl:for-each>};
          
          public Vector myData;

          // agregated vectorSets:
          <xsl:for-each select="vectorSet">
            public Vector <xsl:value-of select="@key"/>Agregated;
          </xsl:for-each>


          <xsl:choose>
            <xsl:when test="local-name(..)='vectorSet'">
              <xsl:variable name="Agregated">
                <xsl:value-of select="$MyObjectClass"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="../@key"/></xsl:call-template><xsl:value-of select="$MyVectorSet"/>
              </xsl:variable>
              //agregated Class
              public <xsl:value-of select="$Agregated"/>()
          </xsl:when>
          <xsl:otherwise>
            public <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/>()
          </xsl:otherwise>
        </xsl:choose>
        {
           this.myData=new Vector();
           <xsl:for-each select="vectorSet">
             this.<xsl:value-of select="@key"/>Agregated=new Vector();
           </xsl:for-each>

          } 

          public int getColumnCount()
          {
           return <xsl:value-of select="count(simpleProperty)"/>+1;
          }

          public  int getRowCount()
          {
           return myData.size();
          }

          public String getColumnName(int col)
          {
           if(col==0)
            return "No.";
           else
            return simplePropertyNames[col-1];
          }

          public String getDescription(int col)
          {
           if(col==0)
            return "Counter";
           else
            return simplePropertyDescriptions[col-1];
          }

          public  Class getColumnClass(int col)
          {
           if(col==0)
            return Integer.class;
           else
           {
            Class propClass=simplePropertyClasses[col-1];
            if(propClass==java.util.Date.class)
             return String.class;
            else
	     return propClass;
           }
          }

          public  boolean isCellEditable(int row,int col)
          {
           return (col>0);
          }

          public  Object getValueAt(int row,int col)
          {
           if(col==0)
            return new Integer(row);
           else
           {
            Vector rowData=(Vector)myData.elementAt(row);
            Object value=rowData.elementAt(col-1);
            if(value instanceof java.util.Date)
            {
             DateFormat dateFormat=new SimpleDateFormat(simplePropertyFormats[col-1]);
             return dateFormat.format((Date)value);
            }
            else
             return value;
           }
          }

          public  void setValueAt(Object value,int row,int col)
          {
           if(row<![CDATA[ < ]]>myData.size() <![CDATA[ && ]]> col<![CDATA[ < ]]>getColumnCount())
           {
            Vector rowData=(Vector)myData.elementAt(row);
            if(simplePropertyClasses[col-1]==java.util.Date.class <![CDATA[ && ]]> value instanceof String)
            {
             try
             {
              DateFormat dateFormat=new SimpleDateFormat(simplePropertyFormats[col-1]);
              Date date=dateFormat.parse((String)value);
              rowData.set(col-1,date);
             }
             catch(ParseException e)
             {
              System.out.println("wrong DateFormat, couldn't parse");
             }
            }
            else
             rowData.set(col-1,value);
           }
          }
          public  String getDescription()
          {
           return "This is a VectorSet from a \"<xsl:value-of select="$MyObjectClassName"/>\"-object:\n <xsl:value-of select="description"/>";
          }

          public  String getName()
          {
           return "<xsl:value-of select="name"/>";
          }
          
          // am Anfang der Liste neues Object erzeugen
          public  void createNewObject()
          {
           myData.insertElementAt(newRow(),0);
           <xsl:for-each select="vectorSet">
             <xsl:variable name="Agregated">
               <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
             </xsl:variable>
             <xsl:value-of select="@key"/>Agregated.insertElementAt(new <xsl:value-of select="$Agregated"/>(),0);
           </xsl:for-each>

          }
          
          // am Ende der Liste neues Object erzeugen
          public  void appendNewObject()
          {
           myData.add(newRow());
           <xsl:for-each select="vectorSet">
             <xsl:variable name="Agregated">
               <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
             </xsl:variable>
             <xsl:value-of select="@key"/>Agregated.add(new <xsl:value-of select="$Agregated"/>());
           </xsl:for-each>

          }
          
          public  void insertNewObjectAt(int position)
          {
           myData.insertElementAt(newRow(),position);
           <xsl:for-each select="vectorSet">
             <xsl:variable name="Agregated">
               <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
             </xsl:variable>
             <xsl:value-of select="@key"/>Agregated.insertElementAt(new <xsl:value-of select="$Agregated"/>(),position);
           </xsl:for-each>
          }
        
          public  void removeObject(int position)
          {
           myData.removeElementAt(position);
           <xsl:for-each select="vectorSet">
             <xsl:value-of select="@key"/>Agregated.removeElementAt(position);
           </xsl:for-each>
          }
          
          // von "startIndex" an soundsoviele Objecte loeschen
          public  void removeObjects(int startIndex, int number)
          {
           for(int i=0;i<![CDATA[ < ]]>number;i++)
           {
            myData.removeElementAt(startIndex);
            <xsl:for-each select="vectorSet">
              <xsl:value-of select="@key"/>Agregated.removeElementAt(startIndex);
            </xsl:for-each>
           }
          }

           private Vector newRow()
           {
            Vector newRow=new Vector();
            <xsl:for-each select="simpleProperty">
              newRow.add(null); 
            </xsl:for-each>
            return newRow;
           }

            /*
            is never called, conversion of date not implemented also
            public void setSimplePropertyOfLastRow(String propKey,String value)
            {
             Vector lastRow=(Vector)myData.lastElement();
             <xsl:for-each select="simpleProperty">
               if("<xsl:value-of select="@key"/>".equals(propKey))
                lastRow.setElementAt(new <xsl:value-of select="type"/>(value),<xsl:value-of  select="position()"/>);
             </xsl:for-each>
             }
             */
             
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

            public void toXml(Writer out) throws IOExcpetion
            {
             Tools.genXmlTag(out,"v","key","<xsl:value-of select="$myVectorSet"/>");

             for(int row=0;row<![CDATA[ < ]]>myData.size();row++)
             {
              Vector rowData=(Vector)myData.elementAt(row);
              Object[] simpleProps=
              {
               <xsl:for-each select="simpleProperty">
                 <xsl:choose>
                   <xsl:when test="type='java.util.Date'">
                     date2Millis(rowData.elementAt(<xsl:value-of select="position()-1"/>))
                   </xsl:when>
                   <xsl:otherwise>
                     rowData.elementAt(<xsl:value-of select="position()-1"/>)
                   </xsl:otherwise>
                 </xsl:choose>
                 <xsl:if test="position() != last()">,</xsl:if>
               </xsl:for-each>};
               Tools.genXmlOpenTag(out,"v_row",simplePropertyKeys,simpleProps);

               <xsl:for-each select="vectorSet">
                 <xsl:variable name="Agregated">
                   <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
                 </xsl:variable>
                 Tools.genXmlTag(out,"aggregated");
                 ((de.tuhh.wb.javagis.model.GisToXmlConvertable)<xsl:value-of select="@key"/>Agregated.elementAt(row)).toXml(out);
                 Tools.genXmlTag(out,"/aggregated");
               </xsl:for-each>
              Tools.genXmlTag(out,"/v_row");
              }
             Tools.genXmlTag(out,"/v");
            }


               public boolean hasVectorSets()
               {
                return (<xsl:value-of select="count(vectorSet)"/><![CDATA[ > ]]>0);
               }

               public Vector getVectorSetTableModels(int row)
               {
                Vector result=new Vector();
                <xsl:for-each select="vectorSet">
                  result.add(<xsl:value-of select="@key"/>Agregated.elementAt(row));
                </xsl:for-each>
                return result;
               }


                public void loadFromGisTransferObject(GisTransferObject gto)
                {
                 VectorSet vectorSet=gto.getVectorSet("<xsl:value-of select="@key"/>");
                 if(vectorSet!=null)
                  loadFromVectorSetTransferObject(vectorSet);
                }

            public VectorSet toVectorSetTransferObject()
            {
             VectorSet vs=new VectorSet("<xsl:value-of select="$myVectorSet"/>");   

             for(int row=0;row<![CDATA[ < ]]>myData.size();row++)
             {
              Vector rowData=(Vector)myData.elementAt(row);
              Object[] simpleProps=
              {
               <xsl:for-each select="simpleProperty">
                 <xsl:choose>
                   <xsl:when test="type='java.util.Date'">
                     date2Millis(rowData.elementAt(<xsl:value-of select="position()-1"/>))
                   </xsl:when>
                   <xsl:otherwise>
                     rowData.elementAt(<xsl:value-of select="position()-1"/>)
                   </xsl:otherwise>
                 </xsl:choose>
                 <xsl:if test="position() != last()">,</xsl:if>
               </xsl:for-each>};
               vs.addRow(simplePropertyKeys,simpleProps);
               //Tools.genXmlOpenTag(out,"v_row",simplePropertyKeys,simpleProps);

               <xsl:for-each select="vectorSet">
                 <xsl:variable name="Agregated">
                   <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
                 </xsl:variable>
                 VectorSet vsAgregated=((de.tuhh.wb.javagis.model.GisToXmlConvertable)Agregated.elementAt(row)).toVectorSetTransferObject();
                 vs.addVectorSet(vsAgregated);
                 //                 Tools.genXmlTag(out,"aggregated");
                 //                 ((de.tuhh.wb.javagis.model.GisToXmlConvertable)<xsl:value-of select="@key"/>Agregated.elementAt(row)).toXml(out);
                 //                 Tools.genXmlTag(out,"/aggregated");
               </xsl:for-each>
               //              Tools.genXmlTag(out,"/v_row");
              }
               //             Tools.genXmlTag(out,"/v");
               return vs;
            }











                public void loadFromVectorSetTransferObject(VectorSet vectorSet)
                {
                String value;
                Vector aggregatedVectorSets;
                VectorSet aggregatedVectorSet;
                java.util.GregorianCalendar calendar=new java.util.GregorianCalendar();
                for(int row=0;row<![CDATA[ < ]]>vectorSet.size();row++)
                 {
                  // create new Row
                  appendNewObject();
                  // getRow
                  int lastRow=getRowCount()-1;
                  // simpleProperties:
                  <xsl:for-each select="simpleProperty">
                    value=vectorSet.getSimpleProperty("<xsl:value-of select="@key"/>",row);
                    if(value!=null)
                    {
                    <xsl:choose>
                      <xsl:when test="type='java.util.Date'">
                        try
                        {
                        calendar.setTimeInMillis(java.lang.Long.parseLong(value));
                        Object valueObject=(Object)calendar.getTime();
                        setValueAt(valueObject,lastRow,<xsl:value-of select="position()"/>);
                        }
                        catch(Exception e)
                        {
                         System.out.println("problem parsing date (not in longformat?)");
                         System.out.println(e.getMessage());
                        }
                      </xsl:when>
                      <xsl:otherwise>
                         Object valueObject=(Object)new <xsl:value-of select="type"/>(value);
                         setValueAt(valueObject,lastRow,<xsl:value-of select="position()"/>);
                      </xsl:otherwise>
                    </xsl:choose>

                    }
                  </xsl:for-each>


                  <xsl:if test="vectorSet">
                    aggregatedVectorSets=getVectorSetTableModels(row);
                  </xsl:if>
                  // vectorSets:
                  <xsl:for-each select="vectorSet">
                    <xsl:variable name="Agregated">
                      <xsl:value-of select="$MyObjectClass"/><xsl:value-of select="$MyVectorSet"/><xsl:call-template name="toUpperCase"><xsl:with-param name="word" select="@key"/></xsl:call-template>               
                    </xsl:variable>
                    aggregatedVectorSet=vectorSet.getVectorSet("<xsl:value-of select="@key"/>",row);
                    if(aggregatedVectorSet!=null)
                     ((<xsl:value-of select="$Agregated"/>)aggregatedVectorSets.elementAt(<xsl:value-of select="position()-1"/>)).loadFromVectorSetTransferObject(aggregatedVectorSet);

                  </xsl:for-each>
                  }
             }
          }
        </xsl:for-each>
      </xsl:for-each>
            
       </xsl:template>
       
       <xsl:template name="toUpperCase">
         <xsl:param name="word"/>
         <xsl:value-of select="translate(substring($word,1,1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')" />
         <xsl:value-of select="substring($word,2)"/>
       </xsl:template>
       
     </xsl:stylesheet>
     