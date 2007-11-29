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
package org.kalypso.kalypsomodel1d2d.conv.results;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

public class ResultMeta1d2dFileHelper
{
  /**
   * removes the specified resultMeta file
   */
  private static void removeResultMetaFile( final IResultMeta resultMeta )
  {
    final IPath resultPath = resultMeta.getFullPath();

    final IResource member = ResourcesPlugin.getWorkspace().getRoot().findMember( resultPath );

    try
    {
      if( member != null )
        member.delete( true, new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * removes the specified resultMeta file including all of its children
   */
  public static void removeResultMetaFileWithChidren( final IResultMeta resultMeta ) throws CoreException
  {
    final IFeatureWrapperCollection<IResultMeta> children = resultMeta.getChildren();

    /* delete children */
    for( IResultMeta child : children )
    {
      removeResultMetaFileWithChidren( child );
    }

    /* delete parent */
    removeResultMetaFile( resultMeta );

  }

  /**
   * Refreshes the file structure in dependence of the resultMeta entries. All files that are not included in the
   * resultMeta will be deleted.
   */
  public static void refreshScenario( final IScenarioResultMeta scenarioResultMeta )
  {
    // TODO
  }

}
