/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.featureview;

import java.util.ArrayList;
import java.util.Collection;

import org.deegree.model.feature.event.ModellEventProvider;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author belger
 */
public class FeatureviewDialog extends Dialog implements ModifyListener
{
  private final FeatureComposite m_featureComposite;

  private static final int APPLY_ID = IDialogConstants.CLIENT_ID + 1;

  private static final int RESET_ID = IDialogConstants.CLIENT_ID + 2;

  private final ModellEventProvider m_eventprovider;

  private final ICommandTarget m_commandTarget;

  public FeatureviewDialog( final Shell parentShell, final ModellEventProvider eventprovider, final FeatureComposite factory, final ICommandTarget commandTarget )
  {
    super( parentShell );

    m_eventprovider = eventprovider;
    m_featureComposite = factory;
    m_commandTarget = commandTarget;
  }
  
  

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    getShell().setText( "Feature editieren" );

    final Control control = m_featureComposite.createControl( parent, SWT.BORDER );
    
    m_featureComposite.addModifyListener( this );
    
    return control;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  protected void createButtonsForButtonBar( Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    createButton( parent, APPLY_ID, "�bernehmen", false );
    createButton( parent, RESET_ID, "Zur�cksetzen", false );
    
    updateButtons( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
   */
  protected void buttonPressed( int buttonId )
  {
    switch( buttonId )
    {
    case APPLY_ID:
      applyPressed();
      break;

    case RESET_ID:
      resetPressed();
      break;

    default:
      super.buttonPressed( buttonId );
    }

  }

  private void applyPressed()
  {
    final Collection changes = new ArrayList();
    m_featureComposite.collectChanges( changes );
    
    m_commandTarget.postCommand( new ChangeFeaturesCommand( m_eventprovider, (FeatureChange[])changes.toArray( new FeatureChange[changes.size()] ) ), null );
    
    updateButtons( false );
  }

  private void resetPressed()
  {
    m_featureComposite.updateControl();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    applyPressed();
    
    m_featureComposite.removeModifyListener( this );
    
    super.okPressed();
  }
  
  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  protected void cancelPressed()
  {
    m_featureComposite.removeModifyListener( this );

    super.cancelPressed();
  }
  
  
  private void updateButtons( final boolean bEnable )
  {
    getButton( IDialogConstants.OK_ID ).setEnabled( bEnable );
    getButton( APPLY_ID ).setEnabled( bEnable );
    getButton( RESET_ID ).setEnabled( bEnable );
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    final Collection changes = new ArrayList();
    m_featureComposite.collectChanges( changes );
    
    final boolean bDirty = changes.size() != 0;

    updateButtons( bDirty );
  }
}