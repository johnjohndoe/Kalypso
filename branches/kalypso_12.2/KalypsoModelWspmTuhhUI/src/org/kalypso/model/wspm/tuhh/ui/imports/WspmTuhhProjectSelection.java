/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.tuhh.ui.imports;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.part.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class WspmTuhhProjectSelection
{
  private final CommandableWorkspace m_workspace;

  private final TuhhWspmProject m_project;

  public WspmTuhhProjectSelection( final ISelection selection )
  {
    final Feature parentFeature = findParentFeature( selection );
    if( parentFeature == null )
    {
      m_workspace = null;
      m_project = null;
      return;
    }

    final IFeatureSelection featureSelection = (IFeatureSelection) selection;
    m_workspace = featureSelection.getWorkspace( parentFeature );

    m_project = findProject( parentFeature );
  }

  private Feature findParentFeature( final ISelection selection )
  {
    if( !(selection instanceof IFeatureSelection) )
      return null;

    final IFeatureSelection featureSelection = (IFeatureSelection) selection;
    final Object firstElement = featureSelection.getFirstElement();
    if( firstElement instanceof Feature )
      return (Feature) firstElement;

    if( !(firstElement instanceof FeatureAssociationTypeElement) )
      return null;

    final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) firstElement;
    return fate.getOwner();
  }

  public boolean hasProject( )
  {
    return m_project != null;
  }

  private TuhhWspmProject findProject( final Feature parentFeature )
  {
    if( parentFeature == null )
      return null;

    if( parentFeature instanceof TuhhWspmProject )
      return (TuhhWspmProject) parentFeature;

    if( GMLSchemaUtilities.substitutes( parentFeature.getFeatureType(), WspmWaterBody.FEATURE_WSPM_WATER_BODY ) )
    {
      final Feature grandDad = parentFeature.getOwner();
      if( grandDad instanceof TuhhWspmProject )
        return (TuhhWspmProject) grandDad;
    }

    return findProject( parentFeature.getOwner() );
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }

  public TuhhWspmProject getProject( )
  {
    return m_project;
  }

}
