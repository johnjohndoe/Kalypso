/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.xml.sax.InputSource;

/**
 * Observation Table Template Handling made easy
 * 
 * @author schlienger
 */
public class TableViewUtils
{
  public final static String OTT_FILE_EXTENSION = "ott";

  private final static ObjectFactory OTT_OF = new ObjectFactory();

  /**
   * Not to be instanciated
   */
  private TableViewUtils( )
  {
    // empty
  }

  /**
   * Loads the xml template from the given reader. Closes the reader.
   * 
   * @return table view template
   * @throws JAXBException
   */
  public static ObstableviewType loadTableTemplateXML( final Reader reader ) throws JAXBException
  {
    try
    {
      return loadTableTemplateXML( new InputSource( reader ) );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * Loads the xml template from the given stream. Closes the stream.
   * 
   * @param ins
   * @return table view template
   * @throws JAXBException
   */
  public static ObstableviewType loadTableTemplateXML( final InputStream ins ) throws JAXBException
  {
    try
    {
      return loadTableTemplateXML( new InputSource( ins ) );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
  
  /**
   * Loads the xml template from the given inutsource
   * 
   * @param ins
   * @return table view template
   * @throws JAXBException
   */
  public static ObstableviewType loadTableTemplateXML( final InputSource ins )
      throws JAXBException
  {
      final ObstableviewType baseTemplate = (ObstableviewType) OTT_OF
          .createUnmarshaller().unmarshal( ins );

      return baseTemplate;
  }

  /**
   * Saves the given template (binding). Closes the stream.
   * 
   * @param xml
   * @param outs
   * @throws JAXBException
   */
  public static void saveTableTemplateXML( final ObstableviewType xml,
      final OutputStream outs ) throws JAXBException
  {
    try
    {
      final Marshaller m = OTT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( xml, outs );
    }
    finally
    {
      IOUtils.closeQuietly( outs );
    }
  }

  /**
   * Saves the given template (binding). Closes the writer.
   * 
   * @param xml
   * @param writer
   * @throws JAXBException
   */
  public static void saveTableTemplateXML( final ObstableviewType xml,
      final Writer writer ) throws JAXBException
  {
    try
    {
      final Marshaller m = OTT_OF.createMarshaller();
      m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      m.marshal( xml, writer );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  /**
   * Builds the xml binding object using the given the table view template
   * 
   * @param template
   * @return xml binding object (ready for marshalling for instance)
   * @throws JAXBException
   */
  public static ObstableviewType buildTableTemplateXML(
      final TableViewTemplate template ) throws JAXBException
  {
    final ObstableviewType xmlTemplate = OTT_OF.createObstableview();

    // rendering rules
    final RulesType xmlRulesType = OTT_OF.createObstableviewTypeRulesType();
    xmlTemplate.setRules( xmlRulesType );
    final List xmlRules = xmlRulesType.getRenderingrule();

    final List rules = template.getRules().getRules();
    for( final Iterator itRules = rules.iterator(); itRules.hasNext(); )
    {
      final RenderingRule rule = (RenderingRule) itRules.next();
      
      final TypeRenderingRule xmlRule = OTT_OF.createTypeRenderingRule();
      xmlRule.setMask( rule.getMask() );
      if( rule.getForegroundColor() != null )
        xmlRule.setForegroundcolor( StringUtilities.colorToString( rule.getForegroundColor() ) );
      if( rule.getBackgroundColor() != null )
        xmlRule.setBackgroundcolor( StringUtilities.colorToString( rule.getBackgroundColor() ) );
      if( rule.getFont() != null )
        xmlRule.setFont( StringUtilities.fontToString( rule.getFont() ) );
      xmlRule.setTooltip( rule.getTooltipText() );
      
      xmlRules.add( xmlRule );
    }
    
    // themes
    final List xmlObsList = xmlTemplate.getObservation();
    
    int colCount = 0;
    
    final Collection themes = template.getThemes();
    for( final Iterator itThemes = themes.iterator(); itThemes.hasNext(); )
    {
      final TableViewTheme theme = (TableViewTheme) itThemes.next();
      
      final IObservation obs = theme.getObservation();
      if( obs == null )
        continue;
      
      final TypeObservation xmlObs = OTT_OF.createTypeObservation();
      xmlObs.setHref( obs.getHref() );
      xmlObs.setLinktype( "zml" );

      xmlObsList.add( xmlObs );
      
      // columns
      final List xmlColumns = xmlObs.getColumn();
      
      final List columns = theme.getColumns();
      for( Iterator itCol = columns.iterator(); itCol.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn) itCol.next();
        
        colCount++;
        final TypeColumn xmlCol = OTT_OF.createTypeColumn();
        xmlCol.setAxis( col.getAxis().getName() );
        xmlCol.setEditable( col.isEditable() );
        xmlCol.setId( "c" + String.valueOf( colCount ) );
        xmlCol.setName( col.getName() );
        xmlCol.setWidth( col.getWidth() );
        
        xmlColumns.add( xmlCol );
      }
    }
    
    return xmlTemplate;
  }
}