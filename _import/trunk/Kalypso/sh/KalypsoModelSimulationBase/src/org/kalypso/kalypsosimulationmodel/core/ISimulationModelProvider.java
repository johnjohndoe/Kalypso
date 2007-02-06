/**
 * 
 */
package org.kalypso.kalypsosimulationmodel.core;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;

/**
 * Interface to be implemented class to act as simulation model provider. This is use e.g. in the worksflow system
 * 
 * @author Patrice Congo
 * @author Gernot Belger
 * @author Stefan Kurzbach
 */
public interface ISimulationModelProvider
{
  /**
   * To get the simulation model
   * 
   * @return the simulation model as {@link IFeatureWrapper}
   */
  IFeatureWrapper getSimulationModel( );

  IStructuredSelection getSelection( );

  Shell getShell( );
}
