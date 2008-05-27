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
package org.kalypso.model.wspm.sobek.core.ui.lastfall;

import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.TreeItem;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kuch
 */
public class LastfallTreeLabelProvider extends LabelProvider
{

  private final TreeViewer m_viewer;

  public LastfallTreeLabelProvider( final TreeViewer viewer )
  {
    m_viewer = viewer;
    // TODO Auto-generated constructor stub
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
   */
  @Override
  public Image getImage( final Object element )
  {
    if( element instanceof ILastfall )
      return new Image( null, getClass().getResourceAsStream( "icons/tree_lastfall.gif" ) ); //$NON-NLS-1$

    return new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( final Object element )
  {
    if( element instanceof ILastfall )
    {
      final ILastfall lastfall = (ILastfall) element;

      return lastfall.getName();
    }
    else if( element instanceof INode )
    {
      final INode node = (INode) element;

      return String.format( "%s %s", Messages.LastfallTreeLabelProvider_3, node.getName() );
    }

    return super.getText( element );
  }

  public void updateIcons( )
  {
    final TreeItem[] items = m_viewer.getTree().getItems();

    updateTreeItemIcons( items );

  }

  private void updateTreeItemIcons( final TreeItem[] items )
  {
    for( final TreeItem item : items )
    {
      updateTreeItemIcons( item.getItems() );

      if( item.getData() instanceof IBoundaryNode )
        try
        {
          final IBoundaryNode node = (IBoundaryNode) item.getData();
          final ILastfall lastfall = (ILastfall) item.getParentItem().getData();

          final IBoundaryNodeLastfallCondition condition = node.getLastfallCondition( lastfall );

          final BOUNDARY_CONDITION_TYPE type = condition.getLastUsedType();
          if( type == null )
            item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$

          // FIXME - wait for eclipse 3.4 and show tooltip text of tree items (why is open / done, etc)

          if( IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eZml.equals( type ) )
          {
            // REMARK: empty obs throws Nullpointer exception!
            final IObservation<TupleResult> observation = condition.getTimeSeriesObservation();
            final TupleResult result = observation.getResult();
            if( result.size() > 2 )
            {

              final BOUNDARY_TYPE boundaryType = condition.getBoundaryNode().getBoundaryType();
              if( BOUNDARY_TYPE.eWQ.equals( boundaryType ) )
              {
                if( result.size() > 0 )
                  item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_done.gif" ) ) ); //$NON-NLS-1$
                else
                  item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
              }
              else
              {
                /* determine date range of observation */
                final IRecord recordStart = result.get( 0 );
                final XMLGregorianCalendar xmlStart = (XMLGregorianCalendar) recordStart.getValue( 0 );

                final IRecord recordEnd = result.get( result.size() - 1 );
                final XMLGregorianCalendar xmlEnd = (XMLGregorianCalendar) recordEnd.getValue( 0 );

                final GregorianCalendar lastfallStart = lastfall.getLastfallStart();
                final GregorianCalendar lastfallEnd = lastfall.getLastfallEnd();

                final Integer preSimulationTime = lastfall.getPreSimulationTime();
                lastfallStart.add( Calendar.HOUR, preSimulationTime * -1 );

                final GregorianCalendar start = xmlStart.toGregorianCalendar();
                final GregorianCalendar end = xmlEnd.toGregorianCalendar();

                if( start.after( lastfallStart ) )
                {
                  item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
                }
                else if( end.before( lastfallEnd ) )
                {
                  item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
                }
                else
                {
                  item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_done.gif" ) ) ); //$NON-NLS-1$
                }
              }
            }

            else
              item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
          }
          else if( IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eConstant.equals( type ) )
          {
            final Double value = condition.getConstantValue();

            if( value == null )
              item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
            else
              item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_done.gif" ) ) ); //$NON-NLS-1$

          }
          else
            item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
        }
        catch( final Exception e )
        {
          item.setImage( new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) ) ); //$NON-NLS-1$
        }
    }

  }

// /**
// * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
// */
// @Override
// public Image getImage( final Object element )
// {
//
// if( element instanceof ILastfall )
// return new Image( null, getClass().getResourceAsStream( "icons/tree_lastfall.gif" ) );
// else if( element instanceof IBoundaryNode )
// {
// final IBoundaryNode node = (IBoundaryNode) element;
// try
// {
// final ILastfall lastfall = getParentLastfall( m_viewer.getTree().getItems(), node );
// if( lastfall == null )
// return super.getImage( element );
//
// final IBoundaryNodeLastfallCondition condition = node.getLastfallCondition( lastfall );
//
// final BOUNDARY_CONDITION_TYPE type = condition.getLastUsedType();
// if( type == null )
// return new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) );
//
// if( IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eZml.equals( type ) )
// {
// // REMARK: empty obs throws Nullpointer exception!
// condition.getTimeSeriesObservation();
//
// return new Image( null, getClass().getResourceAsStream( "icons/tree_done.gif" ) );
// }
// else if( IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE.eConstant.equals( type ) )
// {
// final Double value = condition.getConstantValue();
//
// if( value == null )
// return new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) );
// else
// return new Image( null, getClass().getResourceAsStream( "icons/tree_done.gif" ) );
//
// }
// else
// return new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) );
// }
// catch( final Exception e )
// {
// return new Image( null, getClass().getResourceAsStream( "icons/tree_open.gif" ) );
// }
//
// }
//
// return super.getImage( element );
// }
//
// private ILastfall getParentLastfall( final TreeItem[] items, final IBoundaryNode node )
// {
// for( final TreeItem item : items )
// {
// final Object data = item.getData();
// if( data.equals( node ) )
// return (ILastfall) item.getParentItem().getData();
//
// final ILastfall lastfall = getParentLastfall( item.getItems(), node );
//
// if( lastfall == null )
// return getParentLastfall( item.getItems(), node );
// else
// return lastfall;
// }
//
// return null;
// }
}
