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
package org.kalypso.ui.rrm.internal.simulations.actions;

import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * This actions opens the error.gml.
 * 
 * @author Holger Albert
 */
public class OpenErrorGmlAction extends Action implements IUpdateable
{
  /**
   * The simulation.
   */
  private final RrmCalculationResult m_result;

  /**
   * The constructor.
   * 
   * @param text
   *          The text.
   * @param tooltipText
   *          The tooltip text.
   * @param simulation
   *          The simulation.
   */
  public OpenErrorGmlAction( final String text, final String tooltipText, final RrmCalculationResult result )
  {
    super( text );

    setToolTipText( tooltipText );
    m_result = result;
  }

  @Override
  public void run( )
  {
    try
    {
      /* Get the file. */
      final IFile errorGml = m_result.getErrorGml();

      /* Check if the file exists. */
      if( !errorGml.exists() )
        throw new IOException( String.format( Messages.getString( "OpenErrorGmlAction_0" ), errorGml.getName() ) ); //$NON-NLS-1$

      // TODO
    }
    catch( final Exception ex )
    {
      /* Display the error. */
      final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
      final String dialogTitle = getText();
      final String message = Messages.getString( "OpenErrorGmlAction_1" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), ex.getLocalizedMessage(), ex );
      ErrorDialog.openError( shell, dialogTitle, message, status );
    }
  }

  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.OPEN_ERROR_GML_ACTION );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.IUpdateable#update()
   */
  @Override
  public void update( )
  {
    final IFile errorGml = m_result.getErrorGml();
    if( errorGml.exists() )
      setEnabled( true );
    else
      setEnabled( false );
  }
}