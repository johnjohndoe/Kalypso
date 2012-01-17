package org.kalypso.model.wspm.tuhh.ui.actions.simplify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.base.IProfileManipulator;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

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
  public void performProfileManipulation( final IProfil profile, final IProgressMonitor monitor )
  {
    monitor.beginTask( "", 1 );//$NON-NLS-1$

    final IProfileRecord[] points = m_simplifyPage.getSelectedPoints( profile );

    final IProfileRecord[] pointsToKeep = getPointsToKeep( profile );

    final IProfileRecord[] pointsToRemove = DouglasPeuckerHelper.reducePoints( points, pointsToKeep, m_allowedDistance );

    profile.getResult().removeAll( Arrays.asList( pointsToRemove ) );

    monitor.done();
  }

  private IProfileRecord[] getPointsToKeep( final IProfil profile )
  {
    final Collection<IProfileRecord> pointsToKeep = new ArrayList<IProfileRecord>();

    /* Marked points (TF, DB, BV) should never get simplified */
    final IProfileRecord[] markedPoints = profile.getMarkedPoints();
    pointsToKeep.addAll( Arrays.asList( markedPoints ) );

    /*
     * TODO: in order to do a correct simplifikatino with bridges/weirs, we need to split up the simplifikation in
     * several chunks per bridge-part.
     */

    /* for now, we just keep all bridge points, which does not work if all points are bridge points */
    final IProfileRecord[] buildingPoints = getBuildingPoints( profile );
    pointsToKeep.addAll( Arrays.asList( buildingPoints ) );

    return pointsToKeep.toArray( new IProfileRecord[pointsToKeep.size()] );
  }

  private IProfileRecord[] getBuildingPoints( final IProfil profile )
  {
    final Collection<IProfileRecord> buildingPoints = new ArrayList<IProfileRecord>();

    final String[] buildingComponents = getBuildingComponents();
    for( final String buildingComponent : buildingComponents )
    {
      final IProfileRecord[] componentPoints = getBuildingPoints( profile, buildingComponent );
      buildingPoints.addAll( Arrays.asList( componentPoints ) );
    }

    return buildingPoints.toArray( new IProfileRecord[buildingPoints.size()] );
  }

  private IProfileRecord[] getBuildingPoints( final IProfil profile, final String buildingComponent )
  {
    if( m_keepBuildingPoints )
      return getAllValidPoints( profile, buildingComponent );

    return getStartingEndBuildingPoints( profile, buildingComponent );
  }

  /**
   * Get all profile points of a component, which values are of type number (i.e non-<code>null</code>).
   */
  private IProfileRecord[] getAllValidPoints( final IProfil profile, final String buildingComponent )
  {
    final Collection<IProfileRecord> allPoints = new ArrayList<IProfileRecord>();

    final int componentIndex = profile.indexOfProperty( buildingComponent );
    if( componentIndex == -1 )
      return new IProfileRecord[0];

    final IProfileRecord[] points = profile.getPoints();
    for( final IProfileRecord point : points )
    {
      final Object value = point.getValue( componentIndex );
      if( value instanceof Number )
      {
        allPoints.add( point );
      }
    }

    return allPoints.toArray( new IProfileRecord[allPoints.size()] );
  }

  // FIXME: does not work properly we need to consider if the point lies on the soil or not, see BridgeRule
  @SuppressWarnings("unused")
  private IProfileRecord[] getStartingEndBuildingPoints( final IProfil profile, final String buildingComponent )
  {
    final Collection<IProfileRecord> startOrEndPoints = new HashSet<IProfileRecord>();

// final int componentIndex = profile.indexOfProperty( buildingComponent );
// if( componentIndex == -1 )
// return new IRecord[0];
//
// final IRecord[] points = profile.getPoints();
//
// boolean lastValid = false;
// IRecord lastPoint = null;
// for( final IRecord point : points )
// {
// final Object value = point.getValue( componentIndex );
// final boolean isValid = value instanceof Number;
//
// /* Start point */
// if( isValid && !lastValid )
// startOrEndPoints.add( point );
//
// if( !isValid && lastValid && lastPoint != null )
// startOrEndPoints.add( lastPoint );
//
// lastValid = isValid;
// lastPoint = point;
// }

    return startOrEndPoints.toArray( new IProfileRecord[startOrEndPoints.size()] );
  }

  private String[] getBuildingComponents( )
  {
    return new String[] { IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };
  }
}