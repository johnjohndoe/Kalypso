package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.util.Date;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureBinding;
import org.kalypso.model.wspm.core.imports.ImportTrippleHelper;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Gernot Belger
 */
final class TrippleImportOperation extends AbstractImportProfileOperation
{
  private static final DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.MEDIUM, DateFormat.SHORT );

  private final IRiverProfileNetworkCollection m_profNetworkColl;

  private final List<Feature> m_terrainModelAdds;

  private final File m_trippelFile;

  private final String m_separator;

  private final String m_crs;

  private IRiverProfileNetwork m_network;

  TrippleImportOperation( final ImportProfileData data, final File trippelFile, final String separator, final String crs )
  {
    super( data );

    m_trippelFile = trippelFile;
    m_separator = separator;
    m_crs = crs;
    m_profNetworkColl = data.getProfNetworkColl();
    m_terrainModelAdds = data.getTerrainModelAdds();
  }

  @Override
  protected IStatus execute( final ImportProfileData data, final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.13" ), 2 ); //$NON-NLS-1$

    try
    {
      /* Import Trippel Data */
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.14" ) ); //$NON-NLS-1$

      final IProfil[] profiles = ImportTrippleHelper.importTrippelData( m_trippelFile, m_separator, IWspmTuhhConstants.PROFIL_TYPE_PASCHE, m_crs );

      monitor.worked( 1 );

      /* Convert Trippel Data */
      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.16" ) ); //$NON-NLS-1$

      final IStatus status = doImportNetwork( m_profNetworkColl, m_terrainModelAdds, profiles );

      monitor.worked( 1 );

      return status;
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  @Override
  public Feature[] getTerrainModelAdds( )
  {
    return m_terrainModelAdds.toArray( new Feature[] {} );
  }

  @Override
  public IRiverProfileNetwork getAddedRiverNetwork( )
  {
    return m_network;
  }

  /**
   * Converts the profiles in GML (-> terrain model).
   * 
   * @param networkCollection
   *          the GML river network, in which the profiles will be stored
   * @param addedFeatures
   */
  protected IStatus doImportNetwork( final IRiverProfileNetworkCollection networkCollection, final List<Feature> addedFeatures, final IProfil[] profiles ) throws Exception
  {
    final IRiverProfileNetwork network = networkCollection.getRiverProfileNetworks().addNew( IRiverProfileNetwork.QNAME );
    final Feature networkFeature = network;
    addedFeatures.add( networkFeature );

    /* Set user friendly name and description */
    final String fileName = m_trippelFile.getName();
    final String filePath = m_trippelFile.getAbsolutePath();

    final String desc = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportTrippelWizard.19", fileName, DF.format( new Date() ), filePath ); //$NON-NLS-1$
    network.setName( FileUtilities.nameWithoutExtension( fileName ) );
    network.setDescription( desc );

    final GMLWorkspace workspace = networkFeature.getWorkspace();

    final String coordinatesSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    workspace.accept( new TransformVisitor( coordinatesSystem ), networkFeature, FeatureVisitor.DEPTH_INFINITE );

    for( final IProfil profile : profiles )
    {
      final IProfileFeature profileFeature = (IProfileFeature)FeatureHelper.addFeature( networkFeature, IRiverProfileNetwork.QNAME_PROP_RIVER_PROFILE, IProfileFeature.FEATURE_PROFILE );
      profileFeature.setEnvelopesUpdated();
      ((ProfileFeatureBinding)profileFeature).setProfile( profile );
      profileFeature.setSrsName( m_crs );
      addedFeatures.add( profileFeature );
    }

    final GMLWorkspace workspace2 = networkFeature.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace2, networkFeature, networkFeature, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD );
    workspace.fireModellEvent( event );

    m_network = network;

    return Status.OK_STATUS;
  }
}