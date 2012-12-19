package org.kalypso.model.wspm.tuhh.ui.actions.simplify;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileChange;
import org.kalypso.model.wspm.core.profil.base.IProfileManipulator;
import org.kalypso.model.wspm.core.profil.changes.PointRemove;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.dialogs.reducepoints.IPointsProvider;
import org.kalypso.model.wspm.ui.profil.dialogs.reducepoints.SimplePointsProvider;
import org.kalypso.model.wspm.ui.profil.dialogs.reducepoints.SimplifyProfileOperation;

/**
 * @author Gernot Belger
 */
final class SimplifyProfileManipulator implements IProfileManipulator
{
  private final double m_allowedDistance;

  private final SimplifyProfilePage m_simplifyPage;

  /** If <code>false</code>, only start and end points of buildings are kept. Else all building points are fixed points. */
  private boolean m_keepBuildingPoints = false;

  public SimplifyProfileManipulator( final double allowedDistance, final SimplifyProfilePage simplifyPage )
  {
    m_allowedDistance = allowedDistance;
    m_simplifyPage = simplifyPage;
  }

  public void setKeepBuildingPoints( final boolean keepBuildingPoints )
  {
    m_keepBuildingPoints = keepBuildingPoints;
  }

  @Override
  public Pair<IProfileChange[], IStatus> performProfileManipulation( final IProfile profile, final IProgressMonitor monitor )
  {
    monitor.beginTask( "", 1 );//$NON-NLS-1$

    final IProfileRecord[] points = m_simplifyPage.getSelectedPoints( profile );
    final IPointsProvider provider = new SimplePointsProvider( points );

    final SimplifyProfileOperation operation = new SimplifyProfileOperation( profile, provider, m_allowedDistance, getBuildingComponents() );

    final Pair<IProfileRecord[], IStatus> result = operation.findPointsToRemove();

    final IProfileRecord[] pointsToRemove = result.getKey();
    final IStatus status = result.getValue();

    monitor.done();

    if( pointsToRemove == null )
      return Pair.of( new IProfileChange[0], status );

    final IProfileChange[] changes = new IProfileChange[] { new PointRemove( profile, pointsToRemove ) };
    return Pair.of( changes, status );
  }

  private String[] getBuildingComponents( )
  {
    if( m_keepBuildingPoints )
      return new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };

    return new String[] {};
  }
}