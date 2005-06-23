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
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.MultiStatus;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.ui.KalypsoGisPlugin;
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
  private TableViewUtils()
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
  public static ObstableviewType loadTableTemplateXML( final InputSource ins ) throws JAXBException
  {
    final ObstableviewType baseTemplate = (ObstableviewType)OTT_OF.createUnmarshaller().unmarshal( ins );

    return baseTemplate;
  }

  /**
   * Saves the given template (binding). Closes the stream.
   * 
   * @param xml
   * @param outs
   * @throws JAXBException
   */
  public static void saveTableTemplateXML( final ObstableviewType xml, final OutputStream outs ) throws JAXBException
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
  public static void saveTableTemplateXML( final ObstableviewType xml, final Writer writer ) throws JAXBException
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
  public static ObstableviewType buildTableTemplateXML( final TableView template ) throws JAXBException
  {
    final ObstableviewType xmlTemplate = OTT_OF.createObstableview();

    // rendering rules
    final RulesType xmlRulesType = OTT_OF.createObstableviewTypeRulesType();
    xmlTemplate.setRules( xmlRulesType );
    final List xmlRules = xmlRulesType.getRenderingrule();

    final List rules = template.getRules().getRules();
    for( final Iterator itRules = rules.iterator(); itRules.hasNext(); )
    {
      final RenderingRule rule = (RenderingRule)itRules.next();

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

    final Map map = ObsView.mapItems( template.getItems() );

    for( final Iterator itThemes = map.entrySet().iterator(); itThemes.hasNext(); )
    {
      final Map.Entry entry = (Entry)itThemes.next();
      final IObservation obs = (IObservation)entry.getKey();
      if( obs == null )
        continue;

      final TypeObservation xmlObs = OTT_OF.createTypeObservation();
      xmlObs.setHref( obs.getHref() );
      xmlObs.setLinktype( "zml" );

      xmlObsList.add( xmlObs );

      // columns
      final List xmlColumns = xmlObs.getColumn();

      final List columns = (List)entry.getValue();
      for( Iterator itCol = columns.iterator(); itCol.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn)itCol.next();

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

  public static void applyXMLTemplate( final TableView view, final ObstableviewType xml, final URL context,
      final boolean synchron, final MultiStatus status )
  {
    view.removeAllItems();

    final RulesType trules = xml.getRules();
    if( trules != null )
    {
      // clear the rules since we get ones from the xml
      view.getRules().removeAllRules();

      for( final Iterator it = trules.getRenderingrule().iterator(); it.hasNext(); )
        view.getRules().addRule( RulesFactory.createRenderingRule( (TypeRenderingRule)it.next() ) );
    }

    final List list = xml.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation)it.next();

      TableViewColumnXMLLoader loader;
      try
      {
        loader = new TableViewColumnXMLLoader( view, tobs, context, synchron );
        status.add( loader.getResult() );
      }
      catch( final Throwable e )
      {
        e.printStackTrace();

        status.add( KalypsoGisPlugin.createErrorStatus( "Zeitreihe konnte nicht geladen werden", e ) );
      }

    }
  }
}