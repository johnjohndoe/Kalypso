package org.kalypso.ui.calcwizard;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.deegree.TimeserieFeatureProps;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateFactory;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.impl.DiagramCurve;
import org.kalypso.ogc.sensor.diagview.template.LinkedDiagramCurve;
import org.kalypso.ogc.sensor.tableview.impl.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewColumn;
import org.kalypso.ogc.sensor.tableview.template.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.zml.loader.ZmlLoader;
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
  private KalypsoWizardHelper()
  {
  // not to be instanciated
  }

  /**
   * Parses the properties by fetching all timeserieX elements out (X being
   * whatever, as long as different from element to element). A
   * TimeserieFeatureProps is created for each of these elements.
   * 
   * <pre>
   * <arg name="timeserie1" value="type=...#typeName=...#nameColumn=...#linkColumn=..."/>
   * <arg name="timeserie2" value="type=...#typeName=...#nameColumn=...#linkColumn=..."/>
   * </pre>
   */
  public static TimeserieFeatureProps[] parseTimeserieFeatureProps( final Properties props )
  {
    ArrayList l = new ArrayList();

    Enumeration names = props.propertyNames();
    while( names.hasMoreElements() )
    {
      String pName = (String)names.nextElement();

      if( pName.startsWith( ObservationMapTableDiagWizardPage.PROP_TIMEPROPNAME ) )
        l.add( new TimeserieFeatureProps( PropertiesHelper.parseFromString( props
            .getProperty( pName ), '#' ) ) );
    }

    return (TimeserieFeatureProps[])l.toArray( new TimeserieFeatureProps[0] );
  }

  /**
   * Updates the diagram template for the given TimeserieFeatureProps and
   * features
   */
  public static void updateDiagramTemplate( final TimeserieFeatureProps[] props,
      final List features, final IDiagramTemplate template, final boolean useResolver,
      final IProject project )
  {
    template.removeAllCurves();

    for( Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink)kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
        {
          final Properties mappings = new Properties();
          mappings.setProperty( obsLink.getTimeaxis(), props[i]._diagDateAxis );
          mappings.setProperty( obsLink.getValueaxis(), props[i]._diagValueAxis );

          if( !useResolver )
          {
            final Properties loaderProps = PropertiesHelper
                .parseFromString( obsLink.getHref(), '#' );

            final ZmlLoader loader = new ZmlLoader();
            try
            {
              final IObservation obs = (IObservation)loader.load( loaderProps, project,
                  new NullProgressMonitor() );

              final DiagramCurve curve = new DiagramCurve( name + " (" + props[i]._linkColumn + ')', obs, mappings, template, null );

              template.addCurve( curve );
            }
            catch( LoaderException e )
            {
              // ignored, do nothing...
              //e.printStackTrace();
            }
          }
          else
          {
            final LinkedDiagramCurve curve = new LinkedDiagramCurve( obsLink.getLinktype(),
                new JAXBXLink( obsLink ), name + " (" + props[i]._linkColumn + ')', mappings, template );
            
            template.addCurve( curve );
          }
        }
      }
    }
  }

  /**
   *  
   */
  public static void updateTableTemplate( final TimeserieFeatureProps[] props, final List features,
      final LinkedTableViewTemplate template, final boolean useResolver, final IProject project )
  {
    for( Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink)kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
        {
          if( !useResolver )
          {
            final Properties loaderProps = PropertiesHelper
            .parseFromString( obsLink.getHref(), '#' );
            
            final ZmlLoader loader = new ZmlLoader();
            try
            {
              final IObservation obs = (IObservation)loader.load( loaderProps, project,
                  new NullProgressMonitor() );

              final TableViewColumn col = new TableViewColumn( name + " (" + props[i]._linkColumn + ')', obs, true, 50, obsLink.getTimeaxis(), obsLink
                  .getValueaxis(), null );

              template.addColumn( col );
            }
            catch( LoaderException e )
            {
              // ignored, do nothing...
              //e.printStackTrace();
            }
          }
          else
          {
            final LinkedTableViewColumn col = new LinkedTableViewColumn( name + " (" + props[i]._linkColumn + ')', obsLink
                .getLinktype(), new JAXBXLink( obsLink ), true, 50, obsLink.getTimeaxis(), obsLink
                .getValueaxis() );
            
            template.addColumn( col );
          }
        }
      }
    }
  }

  /**
   * @throws JAXBException
   *  
   */
  public static void updateXMLDiagramTemplate( final TimeserieFeatureProps[] props,
      final List features, final ObsdiagviewType template ) throws JAXBException
  {
    for( Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i]._nameColumn );
        final TimeseriesLink obsLink = (TimeseriesLink)kf
            .getProperty( props[i]._linkColumn );

        if( obsLink != null )
          DiagramTemplateFactory.addTimeseriesLink( template, obsLink, name,
              props[i]._diagDateAxis, props[i]._diagValueAxis );
      }
    }
  }
}