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
package org.kalypso.ogc.gml.om.table.command;

import javax.xml.namespace.QName;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.kalypso.commons.xml.NS;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class AddRowHandler extends AbstractHandler
{
  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final TableViewer viewer = TupleResultCommandUtils.findTableViewer( event );
    final TupleResult tupleResult = TupleResultCommandUtils.findTupleResult( event );
    if( tupleResult == null || viewer == null )
      throw new ExecutionException( "No tuple result viewer available" );

    final IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
    final Object target = selection.getFirstElement();
    final int index = tupleResult.indexOf( target );
    final IRecord next = index < tupleResult.size() - 2 ? tupleResult.get( index + 1 ) : null;
    final IRecord row = tupleResult.createRecord();
    for( final IComponent component : tupleResult.getComponents() )
    {
      if( component.getValueTypeName().equals( new QName( NS.XSD_SCHEMA, "double" ) ) && (target instanceof IRecord) )
      {
        if( "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#BREITE".equals( component.getId() ) )
        {
          if( next != null )
          {
            final Object b1 = ((IRecord) target).getValue( component );
            if( b1 == null )
              continue;
            final Object l = next.getValue( component );
            if( l == null )
              continue;
            row.setValue( component, (Double) b1 - ((Double) b1 - (Double) l) / 2.0 );
          }
          else
          {
            final Object value = ((IRecord) target).getValue( component );
            if( value != null )
              row.setValue( component, (Double) value + 10 );
          }
        }
        else if( "urn:ogc:gml:dict:kalypso:model:wspm:profilePointComponents#HOEHE".equals( component.getId() ) )
        {
          if( next != null )
          {
            final Object h1 = ((IRecord) target).getValue( component );
            if( h1 == null )
              continue;
            final Object z = next.getValue( component );
            if( z == null )
              continue;
            row.setValue( component, (Double) h1 - ((Double) h1 - (Double) z) / 2.0 );
          }
          else
          {
            final Object value = ((IRecord) target).getValue( component );
            if( value != null )
              row.setValue( component, value );
          }
        }
        else
          row.setValue( component, ((IRecord) target).getValue( component ) );
      }
    }
    if( next != null )
      tupleResult.add( index + 1, row );
    else
      tupleResult.add( row );
    return null;
  }
}
