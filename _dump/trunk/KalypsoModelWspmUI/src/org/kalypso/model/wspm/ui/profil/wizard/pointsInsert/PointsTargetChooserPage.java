/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIExtensions;


/**
 * @author Gernot
 * 
 */
public class PointsTargetChooserPage extends WizardPage implements IWizardPage
{
  private static final String DLG_SETTINGS_TARGET_ID = PointsTargetChooserPage.class.getName() + ".dialogSettings.selectedTargetID";

  private final IPointsTarget[] m_targets = KalypsoModelWspmUIExtensions.createProfilPointTargets();

  private IPointsTarget m_selectedTarget;

  public PointsTargetChooserPage( )
  {
    super( "targetChooserPage", "Wählen Sie den Ort, an welchem die Punkte eingefügt werden", null );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final String targetID = getDialogSettings().get( DLG_SETTINGS_TARGET_ID );
    if( targetID == null )
      m_selectedTarget = m_targets.length == 0 ? null : m_targets[0];
    else
    {
      for( final IPointsTarget target : m_targets )
      {
        if( targetID.equals( target.getID() ) )
        {
          m_selectedTarget = target;
          break;
        }
      }
    }

    final Group panel = new Group( parent, SWT.NONE );
    panel.setText( "Hier einfügen: " );
    panel.setLayout( new GridLayout() );

    for( final IPointsTarget target : m_targets )
    {
      final Button button = new Button( panel, SWT.RADIO );
      button.setText( target.getLabel() );
      button.setToolTipText( target.getDescription() );
      button.addSelectionListener( new SelectionAdapter()
      {
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          handleTargetSelected( target );
        }
      } );

      button.setSelection( target == m_selectedTarget );
    }

    setControl( panel );
  }

  @Override
  public void dispose( )
  {
    if( m_selectedTarget != null )
      getDialogSettings().put( DLG_SETTINGS_TARGET_ID, m_selectedTarget.getID() );
    
    super.dispose();
  }

  protected void handleTargetSelected( final IPointsTarget target )
  {
    m_selectedTarget = target;
  }

  public final IPointsTarget getSelectedTarget( )
  {
    return m_selectedTarget;

  }
}
