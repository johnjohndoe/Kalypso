/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypso1d2d.pjt.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;

/**
 * Element of the list of selected restart elements.<br/>
 * Abstraction necessary to correctly handle elements outside the curent scenario.
 * 
 * @author Gernot Belger
 */
class RestartElement
{
  private final IFile m_nodeDocumentFile;

  private final IPath m_restartInfoPath;

  private IDocumentResultMeta m_nodeResult;

  public RestartElement( final IPath restartPath, final IFile nodeDocumentFile )
  {
    m_restartInfoPath = restartPath;
    m_nodeDocumentFile = nodeDocumentFile;
  }

  public IFile getNodeDocumentFile( )
  {
    return m_nodeDocumentFile;
  }

  public IPath getRestartInfoPath( )
  {
    return m_restartInfoPath;
  }

  public void setNodeResult( final IDocumentResultMeta nodeResult )
  {
    m_nodeResult = nodeResult;
  }

  public IDocumentResultMeta getNodeResult( )
  {
    return m_nodeResult;
  }

  public IStepResultMeta getStepResult( )
  {
    if( m_nodeResult == null )
      return null;

    return ResultMeta1d2dHelper.getStepResultMeta( m_nodeResult );
  }

  public boolean isValid( )
  {
    if( m_nodeDocumentFile == null )
      return false;

    return m_nodeDocumentFile.exists();
  }
}