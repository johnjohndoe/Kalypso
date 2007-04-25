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
package org.kalypso.model.wspm.ui.view.table.swt.handlers;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.StringTokenizer;

import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView;

/**
 * @author kimwerner
 */
public class PasteFromClipboardHandler extends AbstractSWTTableHandler implements IHandler
{

  /**
   * @see org.kalypso.model.wspm.ui.view.table.swt.handlers.AbstractSWTTableHandler#doAction(java.lang.String[],
   *      java.util.LinkedList, org.kalypso.model.wspm.ui.view.table.swt.ProfilSWTTableView)
   */
  @Override
  public IStatus doAction( LinkedList<IProfilPoint> selection, ProfilSWTTableView tableView )
  {
    final Clipboard clp = Toolkit.getDefaultToolkit().getSystemClipboard();
    try
    {
      final String content = (String) clp.getData( DataFlavor.stringFlavor );
      if( content != null )
      {
        final Object[] activeCell = tableView.getActiveCell();
        if( ((Double) activeCell[1]).isNaN() )
        {
          final StringTokenizer lines = new StringTokenizer( content, "\n", false );
          final ArrayList<String> propList = new ArrayList<String>();
         final HashMap<String, ArrayList<Double>> propMap = new HashMap<String, ArrayList<Double>>();
          // get PointProperties
          if( lines.hasMoreElements() )
          {
            final StringTokenizer header = new StringTokenizer( lines.nextToken(), "\t", false );

            while( header.hasMoreElements() )
            {
              final String prop = header.nextToken();
              propList.add(prop  );
              propMap.put( prop, new ArrayList<Double>() );
            }
          }
          final String[] pointProperties = propList.toArray( new String[0] );
          // fill the Map
          while( lines.hasMoreElements() )
          {
            final StringTokenizer line = new StringTokenizer( lines.nextToken(), "\t", false );
            final ArrayList<Double> values = new ArrayList<Double>();
            while( line.hasMoreElements() )
            {
              values.add( new Double( line.nextToken() ) );
            }
            final Double[] propertyValues = values.toArray( new Double[0] );
            for( int i = 0; i < propertyValues.length && i < pointProperties.length; i++ )
            {
              final ArrayList<Double> vl = propMap.get( pointProperties[i] );
              vl.add( propertyValues[i] );
            }
          }
          final ArrayList<IProfilChange> changes = new ArrayList<IProfilChange>();
          for( final String prop : selection.getFirst().getProperties() )
          {
            if( propMap.containsKey( prop ) )
            {
              final Double[] values = propMap.get( prop ).toArray( new Double[0] );
              changes.add( new PointPropertyEdit( selection.toArray( new IProfilPoint[0] ), prop, values ) );
            }
          }
          final ProfilOperation operation = new ProfilOperation( "", tableView.getProfilEventManager(), changes.toArray( new IProfilChange[0] ), true );
          new ProfilOperationJob( operation ).schedule();
        }
        else
        {
          final Double value = new Double( content );
          if( value.isNaN() )
            return null;
          tableView.closeCellEditor();
          final IProfilEventManager pem = tableView.getProfilEventManager();
          final IProfil profile = pem.getProfil();
          final ProfilOperation operation = new ProfilOperation( "", tableView.getProfilEventManager(), new PointPropertyEdit( profile.getActivePoint(), (String) activeCell[0], value ), true );
          new ProfilOperationJob( operation ).schedule();
        }
      }
      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      return null;
    }

  }
}
