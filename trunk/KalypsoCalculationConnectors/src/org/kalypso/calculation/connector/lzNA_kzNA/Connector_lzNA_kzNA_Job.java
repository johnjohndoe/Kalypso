package org.kalypso.calculation.connector.lzNA_kzNA;

import java.io.File;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.calculation.connector.IKalypsoModelConnectorType.MODELSPEC_CONNECTOR_LZNA_KZNA;
import org.kalypso.calculation.connector.utils.Connectors;
import org.kalypso.simulation.core.AbstractInternalStatusJob;
import org.kalypso.simulation.core.ISimulation;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.ISimulationResultEater;
import org.kalypso.simulation.core.SimulationException;

public class Connector_lzNA_kzNA_Job extends AbstractInternalStatusJob implements ISimulation
{

  public Connector_lzNA_kzNA_Job( )
  {
  }

  @Override
  public URL getSpezifikation( )
  {
    return getClass().getResource( "resources/modelSpecification.xml" ); //$NON-NLS-N$ //$NON-NLS-1$
  }

  @Override
  public void run( final File tmpdir, final ISimulationDataProvider inputProvider, final ISimulationResultEater resultEater, final ISimulationMonitor monitor ) throws SimulationException
  {
    // final URL urlControlModelKZ = (URL)
    // inputProvider.getInputForID(MODELSPEC_CONNECTOR_LZNA_KZNA.KZNA_CALCULATION.name());
    try
    {
      final URL urlResultFolderLz = Connectors.getURL( inputProvider, MODELSPEC_CONNECTOR_LZNA_KZNA.LZNA_ERGEBNISSE_AKTUEL_ANFANGWERTE.name() );
      final File resultsFolderLZ = new File( urlResultFolderLz.getFile() );
      // final GMLWorkspace workspaceControlModelKZ =
      // GmlSerializer.createGMLWorkspace(urlControlModelKZ, null);
      // final XMLGregorianCalendar calendar = (XMLGregorianCalendar)
      // workspaceControlModelKZ.getRootFeature().getProperty(NaModelConstants.CONTROL_STARTSIMULATION);
      // final String fileName =
      // String.format("%1$tY%1$tm%1$td(%1$tH).gml",
      // calendar.toGregorianCalendar().getTime());

      for( final String resultFileName : resultsFolderLZ.list() )
      {
        if( resultFileName.endsWith( ".gml" ) ) //$NON-NLS-N$ //$NON-NLS-1$
        {
          final File lzSim = new File( resultsFolderLZ, resultFileName );
          final File newOne = new File( tmpdir, "newFile.gml" ); //$NON-NLS-N$ //$NON-NLS-1$
          FileUtils.copyFile( lzSim, newOne );
          if( lzSim.exists() )
          {
            resultEater.addResult( MODELSPEC_CONNECTOR_LZNA_KZNA.KZNA_ANFANGWERTE_LZSIM.name(), newOne );
            return;
          }
        }
      }
    }
    catch( final Exception e )
    {
      throw new SimulationException( e.getLocalizedMessage() );
    }
  }
}