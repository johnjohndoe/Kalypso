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
package org.kalypso.ui.rrm.internal.utils.featureTree;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
public class TreeNodeContentProvider implements ITreeContentProvider
{
  private final ModellEventListener m_modelListener = new ModellEventListener()
  {
    @Override
    public void onModellChange( final ModellEvent modellEvent )
    {
      handleModellChange( modellEvent );
    }
  };

  private ITreeNodeModel m_model;

  private StructuredViewer m_viewer;

  @Override
  public void dispose( )
  {
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = (StructuredViewer) viewer;

    if( oldInput != null )
      ((TreeNodeModel) oldInput).removeModellListener( m_modelListener );

    m_model = (TreeNodeModel) newInput;

    if( m_model != null )
      m_model.addModellListener( m_modelListener );
  }

  @Override
  public Object[] getElements( final Object inputElement )
  {
    return m_model.getRootElements();
  }

  @Override
  public Object getParent( final Object element )
  {
    return ((TreeNode) element).getParent();
  }

  @Override
  public boolean hasChildren( final Object element )
  {
    return ((TreeNode) element).hasChildren();
  }

  @Override
  public Object[] getChildren( final Object parentElement )
  {
    return ((TreeNode) parentElement).getChildren();
  }

  protected void handleModellChange( final ModellEvent modellEvent )
  {
    if( m_model == null )
      return;

    if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      ViewerUtilities.refresh( m_viewer, false );
    }

    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      m_model.clear();

      ViewerUtilities.refresh( m_viewer, false );
    }
  }
}