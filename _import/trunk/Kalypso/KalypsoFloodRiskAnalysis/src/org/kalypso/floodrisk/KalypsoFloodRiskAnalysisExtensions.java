package org.kalypso.floodrisk;


import java.util.Vector;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.kalypso.floodrisk.process.ProcessExtension;

/**
 * ProcessExtensionReader
 * <p>
 * created by
 * 
 * @author Nadja Peiler (13.05.2005)
 */
public class KalypsoFloodRiskAnalysisExtensions
{
  private static String ADDPROCESS_EXTENSION_POINT = "org.kalypso.floodrisk.AddProcess";

  private static String ATT_NAME = "name";

  private static String ATT_SIMULATIONID = "simulationID";

  private static String ATT_ID = "id";

  /**
   * Uses the platform extension registry to retrieve all extensions for the addProcess extension point.
   * <p>
   * For each extension, a ProcessExtension is created
   * 
   * @throws CoreException
   */
  public static ProcessExtension[] retrieveExtensions( )
  {
    final IExtensionRegistry registry = Platform.getExtensionRegistry();

    /*
     * IExtensionPoint[] extPoints = registry.getExtensionPoints(); for (int i = 0;i <extPoints.length;i++){
     * System.out.println(extPoints[i].getUniqueIdentifier()); }
     */
    final IExtensionPoint extensionPoint = registry.getExtensionPoint( ADDPROCESS_EXTENSION_POINT );

    if( extensionPoint == null )
      return new ProcessExtension[0];

    final IExtension[] extensions = extensionPoint.getExtensions();

    final Vector<ProcessExtension> items = new Vector<ProcessExtension>();

    for( int i = 0; i < extensions.length; i++ )
    {
      final IExtension extension = extensions[i];
      final IConfigurationElement[] elements = extension.getConfigurationElements();

      for( int j = 0; j < elements.length; j++ )
      {
        final IConfigurationElement element = elements[j];

        final String name = element.getAttribute( ATT_NAME );
        final String simulationID = element.getAttribute( ATT_SIMULATIONID );
        final String id = element.getAttribute( ATT_ID );

        items.add( new ProcessExtension( name, simulationID, id ) );
      }
    }

    return items.toArray( new ProcessExtension[items.size()] );
  }
}