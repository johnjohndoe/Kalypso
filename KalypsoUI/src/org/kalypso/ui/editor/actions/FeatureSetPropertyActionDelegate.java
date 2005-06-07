/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.actions;

import java.util.HashMap;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IActionDelegate;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.ModifyFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * FeatureRemoveActionDelegate
 * <p>
 * 
 * created by
 * 
 * @author doemming (24.05.2005)
 */
public class FeatureSetPropertyActionDelegate implements IActionDelegate
{

  private ICommandableFeatureSelection m_selection = null;

  private FeatureTypeProperty m_ftp = null;

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    System.out.println( "action remove Feature" );
    if( action.isEnabled() && m_selection != null && m_ftp != null )
    {

      final IKalypsoFeatureTheme theme = m_selection.getKalypsoFeatureTheme();
      final CommandableWorkspace workspace = theme.getWorkspace();
      final FeatureTypeProperty ftp = m_selection.getSelectedFeatureTypeProperty();
      final HashMap map = new HashMap();
      final Object value = m_selection.getSelectedRow().getProperty( ftp.getName() );
      map.put( m_ftp.getName(), value );
      final Feature[] fes = (Feature[])m_selection.toList().toArray( new Feature[m_selection.size()] );
      final ModifyFeatureCommand command = new ModifyFeatureCommand( workspace, fes, map );
      try
      {
        workspace.postCommand( command );
      }
      catch( Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( IAction action, ISelection selection )
  {
    if( selection instanceof ICommandableFeatureSelection && !selection.isEmpty() )
    {
      m_selection = (ICommandableFeatureSelection)selection;
      m_ftp = m_selection.getSelectedFeatureTypeProperty();
      if( m_ftp != null && m_selection.size()>=2)
      {
        action.setEnabled( true );
        String text = action.getText();
        String lang = KalypsoGisPlugin.getDefault().getLang();
        String newText = text.replaceAll( " \\(.*\\)", "" ) + " (" + m_ftp.getAnnotation( lang ).getLabel() + ")";
        action.setText( newText );
        return;
      }
    }
    action.setEnabled( false );
  }
}
