package org.kalypso.ui.calcwizard.modelpages;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.eclipse.core.resources.IFile;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.diagview.impl.DiagramCurve;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewColumn;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.url.UrlResolver;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * Provides some convenience methods for dealing with the stuff in Kalypso
 * Wizards.
 * 
 * @author schlienger
 */
public class KalypsoWizardHelper
{
  //  private static final ObjectFactory m_diagObjectFactory = new
  // ObjectFactory();

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
   *       &lt;arg name=&quot;timeserie1&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   *       &lt;arg name=&quot;timeserie2&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   * </pre>
   * 
   * @param props
   * @return array of TimeserieFeatureProps
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
   * 
   * @param props
   * @param features
   * @param template
   * 
   * @param context
   */
  public static void updateDiagramTemplate( final TimeserieFeatureProps[] props,
      final List features, final LinkedDiagramTemplate template, final URL context )
  {
    template.removeAllCurves();

    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i].getNameColumn() );
        final TimeseriesLink obsLink = (TimeseriesLink)kf.getProperty( props[i].getLinkColumn() );

        if( obsLink != null )
        {
          final Properties mappings = new Properties();
          mappings.setProperty( obsLink.getTimeaxis(), props[i].getDiagDateAxis() );
          mappings.setProperty( obsLink.getValueaxis(), props[i].getDiagValueAxis() );

          final String curveName = name + " (" + props[i].getLinkColumn() + ')';
          final DiagramCurve curve = new DiagramCurve( curveName, null, mappings, template );

          try
          {
            final URL url;
            final UrlResolver resolver = new UrlResolver();
            url = resolver.resolveURL( context, obsLink.getHref() );
            final IFile file = ResourceUtilities.findFileFromURL( url );
            if( file != null && file.exists() )
            {
              final List curves = new ArrayList();
              curves.add( curve );

              final PoolableObjectType key = new PoolableObjectType( obsLink.getLinktype(),
                   obsLink.getHref(), context );
              template.addObservationTheme( key, curves );
            }
          }
          catch( MalformedURLException e )
          {
            e.printStackTrace();
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
   */
  public static void updateTableTemplate( final TimeserieFeatureProps[] props, final List features,
      final LinkedTableViewTemplate template, final URL context )
  {
    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i].getNameColumn() );
        final TimeseriesLink obsLink = (TimeseriesLink)kf.getProperty( props[i].getLinkColumn() );

        if( obsLink != null )
        {
          final String colName = name + " (" + props[i].getLinkColumn() + ')';

          // no observation yet
          final DefaultTableViewColumn col = new DefaultTableViewColumn( colName, true, 50,
              props[i].getDiagValueAxis(), null );

          final PoolableObjectType key = new PoolableObjectType( obsLink.getLinktype(), obsLink
              .getHref(), context );

          final List cols = new ArrayList();
          cols.add( col );

          template.addObservationTheme( key, cols );
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
  public static void updateXMLDiagramTemplate( final TimeserieFeatureProps[] props,
      final List features, final ObsdiagviewType template ) throws JAXBException
  {
    for( Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < props.length; i++ )
      {
        final String name = (String)kf.getProperty( props[i].getNameColumn() );
        final TimeseriesLink obsLink = (TimeseriesLink)kf.getProperty( props[i].getLinkColumn() );

        if( obsLink != null )
          ObservationTemplateHelper.addTimeseriesLink( template, obsLink, name, props[i]
              .getDiagDateAxis(), props[i].getDiagValueAxis() );
      }
    }
  }
}