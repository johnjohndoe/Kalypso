/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.legend;

import java.awt.image.BufferedImage;
import java.util.ArrayList;

import org.kalypsodeegree.filterencoding.Filter;
import org.kalypsodeegree.filterencoding.Operation;
import org.kalypsodeegree.graphics.legend.LegendElement;
import org.kalypsodeegree.graphics.legend.LegendElementCollection;
import org.kalypsodeegree.graphics.legend.LegendException;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.FeatureFilter;
import org.kalypsodeegree_impl.filterencoding.Literal;
import org.kalypsodeegree_impl.filterencoding.LogicalOperation;
import org.kalypsodeegree_impl.filterencoding.OperationDefines;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsCOMPOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsLikeOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyIsNullOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.filterencoding.SpatialOperation;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * 
 * 
 * @version $Revision$
 * @author $author$
 */
public class LegendFactory
{

  private String label = "";

  private String legendtitle = "";

  /**
   * creates a <tt>LegendElement</tt> using the passed <tt>BufferedImage</tt>
   * 
   * @return <tt>LegendElement</tt>
   */
  public LegendElement createLegendElement( BufferedImage legendImage )
  {
    return new LegendElement_Impl( legendImage );
  }

  /**
   * creates a <tt>LegendElement</tt> from a SLD <tt>Style</tt>. Depending
   * on the <tt>Style</tt> the returned <tt>LegendElement</tt> may is a
   * <tt>LegendElementCollection</tt>.
   * 
   * @return <tt>LegendElement</tt>
   */
  public LegendElement createLegendElement( Style style, int width, int height, String title )
      throws LegendException
  {
    Debug.debugMethodBegin();
    setLegendTitle( title );

    if( style instanceof UserStyle )
    {

      LegendElement le = null;
      Rule[] rules = null;
      Filter f = null;
      String propertyname = "";

      FeatureTypeStyle[] fts = ( (UserStyle)style ).getFeatureTypeStyles();
      LegendElementCollection lec = new LegendElementCollection_Impl();

      for( int a = 0; a < fts.length; a++ )
      {
        // legendtitle
        if( getLegendTitle() != null && getLegendTitle().length() > 0 )
        {
          String ttl = getLegendTitle();
          System.out.println( "title: " + ( (UserStyle)style ).getTitle() );
          if( ( (UserStyle)style ).getTitle() != null )
          {
            setLegendTitle( ttl + ": " + ( (UserStyle)style ).getTitle() );
          }
          else
          {
            setLegendTitle( ttl + ": " + ( (UserStyle)style ).getName() );
          }
        }
        else
        {
          setLegendTitle( fts[a].getName() );
        }
        rules = fts[a].getRules();

        for( int b = 0; b < rules.length; b++ )
        {

          if( rules[b].getFilter() != null )
          {
            f = rules[b].getFilter();
            propertyname = getPropertyNameFromFilter( f );
            le = new LegendElement_Impl( new Rule[]
            { rules[b] }, propertyname, 0, 4, true, width, height );
            lec.addLegendElement( le );
          }
          else
          {
            // TODO null oder ""
            // le = new LegendElement_Impl(rules, fts[a].getName(), 0, 4, true,
            // width, height);
            le = new LegendElement_Impl( rules, "", 0, 4, true, width, height );
          }
        }
      }
      // System.out.println(getLegendTitle());
      // lec.setTitle(getLegendTitle());
      // return lec;

      if( lec.getSize() >= 1 )
      {
        lec.setTitle( getLegendTitle() );
        return lec;
      }
      Debug.debugMethodEnd();
      return le;
    }
    throw new LegendException( "LegendFactory: Error in creating the LegendElement:\n"
        + "Given style is not a valid UserStyle." );
  }

  /**
   * creates a <tt>LegendElementCollection</tt> and fills it with the passed
   * <tt>LegendElement</tt>s.
   * 
   * @return <tt>LegendElementCollection</tt>
   */
  public LegendElementCollection createLegendElementCollection( LegendElement[] legendElements )
  {
    Debug.debugMethodBegin( "LegendFactory", "createLegendElementCollection(LegendElement[])" );
    LegendElementCollection lec = new LegendElementCollection_Impl();

    for( int i = 0; i < legendElements.length; i++ )
    {
      lec.addLegendElement( legendElements[i] );
    }
    Debug.debugMethodEnd();
    return lec;
  }

  /**
   * 
   * @param sld
   * @param width
   * @param height
   * @return @throws
   *                LegendException
   */
  public BufferedImage[] createAllThumbnails( StyledLayerDescriptor sld, int width, int height )
      throws LegendException
  {
    Debug.debugMethodBegin( this, "createAllThumbnails" );

    ArrayList list = new ArrayList();

    org.kalypsodeegree.graphics.sld.Layer[] nl = sld.getNamedLayers();
    for( int i = 0; i < nl.length; i++ )
    {
      Style[] styles = nl[i].getStyles();
      for( int j = 0; j < styles.length; j++ )
      {
        if( styles[j] instanceof UserStyle )
        {
          list.add( styles[j] );
        }
      }
    }

    nl = sld.getUserLayers();
    for( int i = 0; i < nl.length; i++ )
    {
      Style[] styles = nl[i].getStyles();
      for( int j = 0; j < styles.length; j++ )
      {
        if( styles[j] instanceof UserStyle )
        {
          list.add( styles[j] );
        }
      }
    }

    LegendElement le = null;
    BufferedImage bi_temp = null; // just temporary
    BufferedImage[] buffi = new BufferedImage[list.size()]; // @return

    for( int i = 0; i < list.size(); i++ )
    {
      Style style = (Style)list.get( i );
      String name = style.getName();
      try
      {
        name = new String( name.replace( ':', '_' ).getBytes(), "UTF-8" );
      }
      catch( Exception e )
      {
        //
      }
      System.out.println( "creating: " + name );
      le = createLegendElement( style, width, height, "" );
      bi_temp = le.exportAsImage();
      buffi[i] = bi_temp;
    }

    Debug.debugMethodEnd();
    return buffi;
  }

  /**
   * gets the property-names for creating the legend text
   */
  private String getPropertyNameFromFilter( Filter filter ) throws LegendException
  {
    Debug.debugMethodBegin( "LegendFactory", "getPropertyNameFromFilter" );
    if( filter instanceof ComplexFilter )
    {
      ComplexFilter cf = (ComplexFilter)filter;
      Operation operation = cf.getOperation();
      String ret = getPropertyNameFromOperation( operation );
      Debug.debugMethodEnd();
      return ret;
    }
    else if( filter instanceof FeatureFilter )
    {
      return "FeatureFilter";
    }
    else
    {
      return "no implementation for " + filter.getClass()
          + " at org.kalypsodeegree_impl.graphics.legend.LegendFactory";
    }
  }

  /**
   * 
   * @param operation
   * @return @throws
   *                LegendException
   */
  private String getPropertyNameFromOperation( Operation operation ) throws LegendException
  {
    Debug.debugMethodBegin( "LegendFactory", "getPropertyNameFromOperation" );

    String legendlabel = "";

    // determines the operation
    // IS COM
    if( operation instanceof PropertyIsCOMPOperation )
    {
      PropertyIsCOMPOperation pCOMPo = (PropertyIsCOMPOperation)operation;
      // gets the PropertyName of the operation for creating a legendtitle
      if( pCOMPo.getFirstExpression() instanceof PropertyName )
      {
        PropertyName propertyname = (PropertyName)pCOMPo.getFirstExpression();
        // setLegendTitleFilterProperty(propertyname.getValue());
        legendlabel += propertyname.getValue();
      }
      else
      {
        throw new LegendException( "LegendElement_Impl: An error occured "
            + "during the parsing of the Filter in the SLD."
            + "First Operation Expression is not of type Literal" );
      }
      legendlabel += getOperationString( pCOMPo.getOperatorId() );
      // gets the Literal of the operation
      if( pCOMPo.getSecondExpression() instanceof Literal )
      {
        Literal literal = (Literal)pCOMPo.getSecondExpression();
        legendlabel += literal.getValue();
      }
      else
      {
        throw new LegendException( "LegendElement_Impl: An error occured "
            + "during the parsing of the Filter in the SLD."
            + "Second Operation Expression is not of type Literal" );
      }
      // LOGICAL
    }
    else if( operation instanceof LogicalOperation )
    {
      LogicalOperation logOp = (LogicalOperation)operation;
      String operatorstring = getOperationString( logOp.getOperatorId() );

      // Operator-ID: AND = 200, OR = 201, NOT = 202
      if( logOp.getOperatorId() == OperationDefines.AND )
      {
        ArrayList andlist = logOp.getArguments();
        String andstring = "";
        for( int i = 0; i < andlist.size(); i++ )
        {
          andstring += getPropertyNameFromOperation( (Operation)andlist.get( i ) );
          if( i < andlist.size() - 1 )
          {
            andstring += operatorstring;
          }
        }
        legendlabel = andstring;
      }
      else if( logOp.getOperatorId() == OperationDefines.OR )
      {
        ArrayList orlist = logOp.getArguments();
        String orstring = "";
        for( int i = 0; i < orlist.size(); i++ )
        {
          orstring += getPropertyNameFromOperation( (Operation)orlist.get( i ) );
          if( i < orlist.size() - 1 )
          {
            orstring += operatorstring;
          }
        }
        legendlabel = orstring;
      }
      else if( logOp.getOperatorId() == OperationDefines.NOT )
      {
        ArrayList notlist = logOp.getArguments();
        String notstring = getPropertyNameFromOperation( (Operation)notlist.get( 0 ) );
        // not is followed by brackets: not (ID = 1 and ID = 2)
        legendlabel = operatorstring + "(" + notstring + ")";
      }

      // SPATIAL
    }
    else if( operation instanceof SpatialOperation )
    {

      SpatialOperation spatop = (SpatialOperation)operation;

      legendlabel = "spatial operation" + spatop;
      // PROPERTY IS LIKE
    }
    else if( operation instanceof PropertyIsLikeOperation )
    {

      PropertyIsLikeOperation prilop = (PropertyIsLikeOperation)operation;

      legendlabel = prilop.getPropertyName().getValue()
          + getOperationString( prilop.getOperatorId() ) + prilop.getLiteral().getValue();
      // LOGICAL
    }
    else if( operation instanceof PropertyIsBetweenOperation )
    {
      PropertyIsBetweenOperation propIsbetween = (PropertyIsBetweenOperation)operation;
      legendlabel = propIsbetween.getPropertyName().getValue()
          + getOperationString( propIsbetween.getOperatorId() ) + propIsbetween.getLowerBoundary()
          + propIsbetween.getUpperBoundary();
    }
    else if( operation instanceof PropertyIsNullOperation )
    {
      PropertyIsNullOperation propertyIsNullOperation = (PropertyIsNullOperation)operation;
      legendlabel = propertyIsNullOperation.getExpression().getExpressionName();
    }
    else
    {
      System.out.println( operation );
      // TODO implement other filter-operations and ELSE!
      throw new LegendException( "Filter-Operation <" + operation.getOperatorName()
          + "> is no PropertyIsCOMPOperation." );
    }

    Debug.debugMethodEnd();
    // System.out.println(legendlabel);
    return legendlabel;

  }

  private String getOperationString( int operationID )
  {
    Debug.debugMethodBegin( "LegendElement_Impl", "getOperationString(int)" );
    String operationString = "";
    // System.out.println("OperationID: " + operationID);
    switch( operationID )
    {
    case OperationDefines.PROPERTYISEQUALTO:
      operationString = " = ";
      break;
    case OperationDefines.PROPERTYISLESSTHAN:
      operationString = " < ";
      break;
    case OperationDefines.PROPERTYISGREATERTHAN:
      operationString = " > ";
      break;
    case OperationDefines.PROPERTYISLESSTHANOREQUALTO:
      operationString = " <= ";
      break;
    case OperationDefines.PROPERTYISGREATERTHANOREQUALTO:
      operationString = " >=  ";
      break;
    case OperationDefines.PROPERTYISLIKE:
      operationString = " is like ";
      break;
    case OperationDefines.PROPERTYISNULL:
      operationString = " is NULL ";
      break;
    case OperationDefines.PROPERTYISBETWEEN:
      operationString = " is between ";
      break;
    case OperationDefines.AND:
      operationString = " and ";
      break;
    case OperationDefines.OR:
      operationString = " or ";
      break;
    case OperationDefines.NOT:
      operationString = " not ";
      break;
    }

    Debug.debugMethodEnd();
    return operationString;
  }

  /**
   * sets the label of the <tt>LegendElement</tt>
   * 
   * @param label
   *                   label of the <tt>LegendElement</tt>
   */
  public void setLabel( String label )
  {
    this.label = label;
  }

  /**
   * returns the label set to <tt>LegendElement</tt>. If no label is set, the
   * method returns <tt>null</tt>
   * 
   * @return label of the <tt>LegendElement</tt> or <tt>null</tt>
   */
  public String getLabel()
  {
    return this.label;
  }

  protected String getLegendTitle()
  {
    return this.legendtitle;
  }

  private void setLegendTitle( String title )
  {
    try
    {
      this.legendtitle = new String( title.getBytes(), "UTF-8" );
    }
    catch( Exception e )
    {
    //  
    }
  }

  /**
   * @return private String getLegendTitleFilterProperty() { return
   *                legendtitlefilterproperty; }
   */

  /**
   * @param string
   * 
   * private void setLegendTitleFilterProperty(String string) {
   * legendtitlefilterproperty = string; }
   */

}

/*******************************************************************************
 * ****************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * LegendFactory.java,v $ Revision 1.19 2004/08/26 12:42:20 poth no message
 * 
 * Revision 1.18 2004/08/10 11:45:57 poth no message
 * 
 * Revision 1.17 2004/08/10 10:31:26 poth no message
 * 
 * Revision 1.16 2004/07/09 07:17:20 poth no message
 * 
 * Revision 1.15 2004/06/01 15:55:05 poth no message
 * 
 * Revision 1.14 2004/05/14 07:45:59 poth no message
 * 
 * Revision 1.13 2004/04/07 10:58:46 axel_schaefer bugfix
 * 
 * 
 *  
 ******************************************************************************/