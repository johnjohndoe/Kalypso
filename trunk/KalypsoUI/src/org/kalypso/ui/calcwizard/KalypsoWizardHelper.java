package org.kalypso.ui.calcwizard;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.kalypso.java.properties.PropertiesHelper;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;

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
   * Parses the properties by fetching all timeserieX elements out (X being whatever, as
   * long as different from element to element). A TimeserieFeatureProps is
   * created for each of these elements.
   * 
   * <pre>
   * <arg name="timeserie1" value="type=...#typeName=...#nameColumn=...#linkColumn=..."/>
   * <arg name="timeserie2" value="type=...#typeName=...#nameColumn=...#linkColumn=..."/>
   * </pre>
   */
  public static TimeserieFeatureProps[] parseTimeserieFeactureProps( final Properties props )
  {
    ArrayList l = new ArrayList();
    
    Enumeration names = props.propertyNames();
    while( names.hasMoreElements() )
    {
      String pName = (String)names.nextElement();
      
      if( pName.startsWith( ObservationMapTableDiagWizardPage.PROP_TIMEPROPNAME ) )
        l.add( new TimeserieFeatureProps( PropertiesHelper.parseFromString( props.getProperty( pName), '#' ) ) );
    }
    
    return (TimeserieFeatureProps[])l.toArray( new TimeserieFeatureProps[0] );
  }

  /**
   * Creates a diagram template for the given TimeserieFeatureProps and features
   * @param baseDiagTemplate
   */
  public static IDiagramTemplate createDiagramTemplate( final TimeserieFeatureProps[] props, final List features, final IFile baseDiagTemplate )
  {
    // TODO
    
    return null;
  }
}
