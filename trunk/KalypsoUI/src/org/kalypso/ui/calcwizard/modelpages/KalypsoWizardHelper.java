package org.kalypso.ui.calcwizard.modelpages;

import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Properties;

import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;

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
   * &lt;arg name=&quot;timeserie1&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   * &lt;arg name=&quot;timeserie2&quot; value=&quot;type=...#typeName=...#nameColumn=...#linkColumn=...&quot;/&gt;
   * </pre>
   * 
   * @param props
   * @return array of TimeserieFeatureProps
   */
  public static TimeserieFeatureProps[] parseTimeserieFeatureProps(
      final Properties props )
  {
    final ArrayList l = new ArrayList();

    final Enumeration names = props.propertyNames();
    while( names.hasMoreElements() )
    {
      final String pName = (String) names.nextElement();

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
   * @param template
   * @param links
   * @param context
   * @param ignoreExceptions
   */
  public static void updateDiagramTemplate(
      final LinkedDiagramTemplate template, final TSLinkWithName[] links,
      final URL context, final boolean ignoreExceptions )
  {
        template.removeAllThemes();

        for( int i = 0; i < links.length; i++ )
        {
          final TSLinkWithName link = links[i];
          template.addObservation( link.name, context, link.href,
              link.linktype, ignoreExceptions, null );
        }
  }

  /**
   * Updates the table template
   * 
   * @param template
   * @param links
   * @param context
   * @param ignoreExceptions
   */
  public static void updateTableTemplate(
      final LinkedTableViewTemplate template, final TSLinkWithName[] links,
      final URL context, final boolean ignoreExceptions )
  {
        template.removeAllThemes();

        for( int i = 0; i < links.length; i++ )
        {
          final TSLinkWithName link = links[i];
          template.addObservation( link.name, context, link.href,
              link.linktype, ignoreExceptions, null );
        }
  }
}