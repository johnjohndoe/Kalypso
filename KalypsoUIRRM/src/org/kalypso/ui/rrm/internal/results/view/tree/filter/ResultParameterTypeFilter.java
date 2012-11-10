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
package org.kalypso.ui.rrm.internal.results.view.tree.filter;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypso.ui.rrm.internal.results.view.base.KalypsoHydrologyResults.RRM_RESULT;
import org.kalypso.ui.rrm.internal.utils.featureTree.TreeNode;

/**
 * @author Dirk Kuch
 */
public class ResultParameterTypeFilter extends AbstractResultViewerFilter
{
  private Object m_type;

  private StructuredViewer m_viewer;

  public static final String PROPERTY_TYPE = "type"; //$NON-NLS-1$

  public ResultParameterTypeFilter( final String string )
  {
    setType( string );
  }

  @Override
  protected boolean doSelect( final TreeNode node )
  {
    final Object type = getType();
    if( StringUtils.EMPTY == type )
      return true;

    if( doSelectNode( node, type ) )
      return true;

    return false;
  }

  public boolean doSelect( final IHydrologyResultReference reference )
  {
    final Object type = getType();
    if( StringUtils.EMPTY == type )
      return true;

    return type.equals( reference.getType() );
  }

  private boolean doSelectNode( final TreeNode node, final Object type )
  {
    if( type instanceof RRM_RESULT )
      return doSelecType( node, (RRM_RESULT)type );

    return false;
  }

  private boolean doSelecType( final TreeNode node, final RRM_RESULT type )
  {
    final int level = getLevel( node );
    if( level == 3 )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelecType( child, type ) )
          return true;
      }

      return false;
    }

    final Object adapter = node.getAdapter( IHydrologyResultReference.class );
    if( !(adapter instanceof IHydrologyResultReference) )
    {
      final TreeNode[] children = node.getChildren();
      for( final TreeNode child : children )
      {
        if( doSelecType( child, type ) )
          return true;
      }

      return false;
    }

    final IHydrologyResultReference reference = (IHydrologyResultReference)adapter;
    if( Objects.notEqual( type, reference.getType() ) )
      return false;

    return reference.isValid();
  }

  public void setViewer( final StructuredViewer viewer )
  {
    m_viewer = viewer;
  }

  public Object getType( )
  {
    return m_type;
  }

  public void setType( final Object type )
  {
    m_type = type;

    if( m_viewer != null )
      m_viewer.refresh();
  }

}
