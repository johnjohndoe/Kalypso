package org.kalypso.ui.calcwizard.modelpages;

import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateFactory;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.template.LinkedDiagramCurve;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewColumn;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.util.xml.xlink.JAXBXLink;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * Provides some convenience methods for dealing with the stuff in Kalypso
 * Wizards.
 * 
 * @author schlienger
 */
public class KalypsoWizardHelper
{
  private KalypsoWizardHelper( )
  {
    // not to be instanciated
  }

  /**
   * Parses the properties by fetching all timeserieX elements out (X being
   * whatever, as long as different from element to element). A
   * TimeserieFeatureProps is created for each of these elements.
   * 
   * <pre>
   * 
   *    &lt;arg name=&quot;timeserie1&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   *    &lt;arg name=&quot;timeserie2&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   *  
   * </pre>
   * 
   * @param props
   * @return array of TimeserieFeatureProps
   */
  public static TimeserieFeatureProps[] parseTimeserieFeatureProps(
      final Properties props )
  {
    ArrayList l = new ArrayList();

    Enumeration names = props.propertyNames();
    while( names.hasMoreElements() )
    {
      String pName = (String) names.nextElement();

      if( pName
          .startsWith( ObservationMapTableDiagWizardPage.PROP_TIMEPROPNAME ) )
        l.add( new TimeserieFeatureProps( PropertiesHelper.parseFromString(
            props.getProperty( pName ), '#' ) ) );
    }

    return (TimeserieFeatureProps[]) l.toArray( new TimeserieFeatureProps[0] );
  }

  /**
   * Updates the diagram template for the given TimeserieFeatureProps and
   * features
   * 
   * @param props
   * @param features
   * @param template
   * 
   * @param context
   * @throws SensorException
   */
  public static void updateDiagramTemplate(
      final TimeserieFeatureProps[] props, final List features,
      final IDiagramTemplate template, final URL context )
      throws SensorException
  {
    template.removeAllCurves();

    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature) it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String) kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink) kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
        {
          final Properties mappings = new Properties();
          mappings.setProperty( obsLink.getTimeaxis(), props[i]._diagDateAxis );
          mappings
              .setProperty( obsLink.getValueaxis(), props[i]._diagValueAxis );

          //          if( !useResolver )
          //          {
          //            final IObservation obs;
          //            try
          //            {
          //              final URL url = UrlResolver.resolveURL( context, obsLink
          //                  .getHref() );
          //
          //              obs = ZmlFactory.parseXML( url, obsLink.getHref() );
          //            }
          //            catch( MalformedURLException e )
          //            {
          //              throw new SensorException( e );
          //            }
          //
          //            final DiagramCurve curve = new DiagramCurve( name + " ("
          //                + props[i]._linkColumn + ')', obs, mappings, template, null );
          //
          //            template.addCurve( curve );
          //          }
          //          else
          {
            final LinkedDiagramCurve curve = new LinkedDiagramCurve( obsLink
                .getLinktype(), new JAXBXLink( obsLink ), name + " ("
                + props[i]._linkColumn + ')', mappings, template, context );

            template.addCurve( curve );
          }
        }
      }
    }
  }

  /**
   * Updates the table template
   * 
   * @param props
   * @param features
   * @param template
   * @param context
   * @throws SensorException
   */
  public static void updateTableTemplate( final TimeserieFeatureProps[] props,
      final List features, final LinkedTableViewTemplate template,
      final URL context ) throws SensorException
  {
    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature) it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String) kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink) kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
        {
          //          if( !useResolver )
          //          {
          //            final IObservation obs;
          //            try
          //            {
          //              final URL url = UrlResolver.resolveURL( context, obsLink
          //                  .getHref() );
          //
          //              obs = ZmlFactory.parseXML( url, obsLink.getHref() );
          //            }
          //            catch( MalformedURLException e )
          //            {
          //              throw new SensorException( e );
          //            }
          //
          //            final TableViewColumn col = new TableViewColumn( name + " ("
          //                + props[i]._linkColumn + ')', obs, true, 50, obsLink
          //                .getTimeaxis(), obsLink.getValueaxis(), null );
          //
          //            template.addColumn( col );
          //          }
          //          else
          {
            final LinkedTableViewColumn col = new LinkedTableViewColumn( template, name
                + " (" + props[i]._linkColumn + ')', obsLink.getLinktype(),
                new JAXBXLink( obsLink ), true, 50, obsLink.getTimeaxis(),
                obsLink.getValueaxis(), context );

            template.addColumn( col );
          }
        }
      }
    }
  }

  /**
   * @param props
   * @param features
   * @param template
   * @throws JAXBException
   */
  public static void updateXMLDiagramTemplate(
      final TimeserieFeatureProps[] props, final List features,
      final ObsdiagviewType template ) throws JAXBException
  {
    for( Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature) it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String) kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink) kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
          DiagramTemplateFactory.addTimeseriesLink( template, obsLink, name,
              props[i]._diagDateAxis, props[i]._diagValueAxis );
      }
    }
  }
}