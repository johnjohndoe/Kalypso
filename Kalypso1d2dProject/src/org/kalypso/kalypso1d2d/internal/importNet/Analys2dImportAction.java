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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectImages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;

/**
 * ASnalyses the imported elements
 * 
 * @author Gernot Belger
 */
public class Analys2dImportAction extends Action implements IUpdateable
{
  private final Import2dElementsData m_data;

  private final Import2dElementsControl m_control;

  public Analys2dImportAction( final Import2dElementsData data, final Import2dElementsControl control )
  {
    m_data = data;
    m_control = control;

    setText( Messages.getString("Analys2dImportAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("Analys2dImportAction_1") ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = Kalypso1d2dProjectPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( Kalypso1d2dProjectImages.DESCRIPTORS.OK ) );
  }

  @Override
  public void update( )
  {
    final boolean enabled = m_data.getElementCount() > 0;
    setEnabled( enabled );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final ICoreRunnableWithProgress operation = new Validate2dImportOperation( m_data.getElements() );

    final IStatus result = ProgressUtilities.busyCursorWhile( operation );
    if( !result.isOK() )
      StatusDialog.open( shell, result, getText() );

    m_control.handleElementsValidated();
  }
}