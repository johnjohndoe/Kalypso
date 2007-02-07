package org.kalypso.kalypsosimulationmodel.core;

import org.eclipse.ui.ISourceProvider;

/**
 * Interface to be implemented to act as simulation model source provider. This is used e.g. in the workflow system
 * 
 * @author Patrice Congo
 * @author Gernot Belger
 * @author Stefan Kurzbach
 */
public interface ISimulationModelProvider extends ISourceProvider
{
  public static final String ACTIVE_SIMULATION_MODEL_NAME = "activeSimulationModel";

  public static final String ACTIVE_SIMULATION_MODEL_BASE_FOLDER_NAME = "activeSimulationModelBaseFolder";
}
