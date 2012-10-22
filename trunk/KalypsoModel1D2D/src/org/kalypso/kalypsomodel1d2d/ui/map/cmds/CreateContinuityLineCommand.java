/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;

import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

public class CreateContinuityLineCommand implements IFeatureChangeCommand
{
  private boolean m_processed = false;

  private IFELine m_line;

  final private IFEDiscretisationModel1d2d m_model;

  final private List<IFE1D2DNode> m_nodeList;

  final private QName m_lineElementQName;

  public CreateContinuityLineCommand( final IFEDiscretisationModel1d2d model, final List<IFE1D2DNode> nodeList, final QName lineElementQName )
  {
    m_model = model;
    m_nodeList = nodeList;
    m_lineElementQName = lineElementQName;
  }

  @Override
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateContinuityLineCommand.0" ); //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    // TODO check if such a line already exists (with same nodes etc...)

    final Feature parentFeature = m_model;
    final GMLWorkspace workspace = parentFeature.getWorkspace();
    final IFeatureBindingCollection<IFELine> continuityLines = m_model.getContinuityLines();

    if( m_lineElementQName.equals( IContinuityLine1D.QNAME ) )
    {
      IContinuityLine1D line1d = continuityLines.addNew( m_lineElementQName, IContinuityLine1D.class );
      line1d.addNode( m_nodeList.get( 0 ) );
      m_line = line1d;
    }
    else
    {
      IContinuityLine2D line2d = continuityLines.addNew( m_lineElementQName, IContinuityLine2D.class );
      line2d.setNodes( m_nodeList );
      m_line = line2d;
    }

    workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, parentFeature, m_line, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    m_processed = true;
  }

  @Override
  public void redo( ) throws Exception
  {
    if( !m_processed )
    {
      process();
    }
  }

  @Override
  public void undo( ) throws Exception
  {
    if( m_processed )
    {
      // TODO remove element and links to it edges and delete element
    }
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { m_line };
  }
}
