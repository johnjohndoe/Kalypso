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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.eclipse.core.runtime.MultiStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.template.obstableview.ObjectFactory;
import org.kalypso.template.obstableview.Obstableview;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.Obstableview.Rules;
import org.kalypso.ui.KalypsoGisPlugin;
import org.xml.sax.InputSource;

/**
 * Observation Table Template Handling made easy
 * 
 * @author schlienger
 */
public final class TableViewUtils
{
  public final static String OTT_FILE_EXTENSION = "ott";

  private final static ObjectFactory OTT_OF = new ObjectFactory();
  private final static JAXBContext OTT_JC = JaxbUtilities.createQuiet( ObjectFactory.class );

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
  public static Obstableview loadTableTemplateXML( final Reader reader ) throws JAXBException
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
   * @return table view template
   */
  public static Obstableview loadTableTemplateXML( final InputStream ins ) throws JAXBException
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
   * Loads the xml template from the given inputsource
   * 
   * @return table view template
   */
  public static Obstableview loadTableTemplateXML( final InputSource ins ) throws JAXBException
  {
    final Obstableview baseTemplate = (Obstableview)OTT_JC.createUnmarshaller().unmarshal( ins );

    return baseTemplate;
  }

  /**
   * Saves the given template (binding). Closes the stream.
   */
  public static void saveTableTemplateXML( final Obstableview xml, final OutputStream outs ) throws JAXBException
  {
    try
    {
      final Marshaller m = OTT_JC.createMarshaller();
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
   */
  public static void saveTableTemplateXML( final Obstableview xml, final Writer writer ) throws JAXBException
  {
    try
    {
      final Marshaller m = OTT_JC.createMarshaller();
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
   * @return xml binding object (ready for marshalling for instance)
   */
  public static Obstableview buildTableTemplateXML( final TableView template )
  {
    final Obstableview xmlTemplate = OTT_OF.createObstableview();

    xmlTemplate.setFeatures( StringUtils.join( template.getEnabledFeatures(), ';' ) );
    
    xmlTemplate.setAlphaSort( template.isAlphaSort() );

    // rendering rules
    final Rules xmlRulesType = OTT_OF.createObstableviewRules();
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
        xmlCol.setFormat( col.getFormat() );

        xmlColumns.add( xmlCol );
      }
    }

    return xmlTemplate;
  }

  public static IStatus applyXMLTemplate( final TableView view, final Obstableview xml, final URL context,
      final boolean synchron, final String ignoreHref )
  {
    view.removeAllItems();

    // features-list is optional
    if( xml.getFeatures() != null )
    {
      final String[] featureNames = xml.getFeatures().split( ";" );
      for( int i = 0; i < featureNames.length; i++ )
        view.setFeatureEnabled( featureNames[i], true );
    }

    view.setAlphaSort( xml.isAlphaSort() );

    final Rules trules = xml.getRules();
    if( trules != null )
    {
      // clear the rules since we get ones from the xml
      view.getRules().removeAllRules();

      for( final Iterator it = trules.getRenderingrule().iterator(); it.hasNext(); )
        view.getRules().addRule( RulesFactory.createRenderingRule( (TypeRenderingRule)it.next() ) );
    }

    final List<IStatus> stati = new ArrayList<IStatus>();

    final List<TypeObservation> list = xml.getObservation();
    final TypeObservation[] tobs = list.toArray(new TypeObservation[list.size()]);
    for( int i = 0; i < tobs.length; i++ )
    {
      // check, if href is ok
      final String href = tobs[i].getHref();

      // Hack: elemente, die durch token-replace nicht richtig aufgelöst werden einfach übergehen
      if( ignoreHref != null && href.indexOf( ignoreHref ) != -1 )
      {
        Logger.getLogger( TableViewUtils.class.getName() ).warning( "Href ignored: " + href );
        continue;
      }
      
      final TableViewColumnXMLLoader loader = new TableViewColumnXMLLoader( view, tobs[i], context, synchron, i );
      stati.add( loader.getResult() );
    }

    return StatusUtilities.createStatus( stati, "Tabellenvorlage konnte nicht vollständig aktualisiert werden" );
  }

  /**
   * Return a map from IObservation to TableViewColumn. Each IObservation is mapped to a list of TableViewColumns which
   * are based on it.
   */
  public static Map<IObservation, ArrayList<TableViewColumn>> buildObservationColumnsMap( final List tableViewColumns )
  {
    final Map<IObservation, ArrayList<TableViewColumn>>  map = new HashMap<IObservation, ArrayList<TableViewColumn>>();

    for( final Iterator it = tableViewColumns.iterator(); it.hasNext(); )
    {
      final TableViewColumn col = (TableViewColumn)it.next();
      final IObservation obs = col.getObservation();

      if( !map.containsKey( obs ) )
        map.put( obs, new ArrayList<TableViewColumn>() );

      map.get( obs ).add( col );
    }

    return map;
  }

  /**
   * Save the dirty observations
   */
  public static IStatus saveDirtyObservations( final List tableViewColumns, final IProgressMonitor monitor )
  {
    final MultiStatus status = new MultiStatus( IStatus.OK, KalypsoGisPlugin.getId(), 0, "Zeitreihen speichern" );

    final Map map = buildObservationColumnsMap( tableViewColumns );

    monitor.beginTask( "Zeitreihen speichern", map.size() );

    for( final Iterator it = map.entrySet().iterator(); it.hasNext(); )
    {
      final Map.Entry entry = (Entry)it.next();
      final IObservation obs = (IObservation)entry.getKey();
      final List cols = (List)entry.getValue();

      boolean obsSaved = false;

      for( final Iterator itCols = cols.iterator(); itCols.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn)itCols.next();

        if( col.isDirty() && !obsSaved )
        {
          try
          {
            KalypsoGisPlugin.getDefault().getPool().saveObject( obs, monitor );

            obsSaved = true;
          }
          catch( final LoaderException e )
          {
            e.printStackTrace();
            status.addMessage( "Fehler beim speichern von " + obs, e );
          }
        }

        col.setDirty( false, null );
      }

      monitor.worked( 1 );
    }

    return status;
  }
}