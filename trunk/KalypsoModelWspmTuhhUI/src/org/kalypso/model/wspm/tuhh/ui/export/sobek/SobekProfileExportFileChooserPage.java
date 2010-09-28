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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.ui.forms.MessageProvider;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.export.ValidatingWizardPage;
import org.kalypso.model.wspm.tuhh.ui.export.wspwin.ProfilePatternInputReplacer;

/**
 * @author Gernot Belger
 */
public class SobekProfileExportFileChooserPage extends ValidatingWizardPage
{
  private static final String DEF_GROUP_LABEL = "'profile.def' Datei";

  private static final String DEF_FILTER_LABEL = "SOBEK Profile.def File";

  // FIXME
  private static final String FRIC_GROUP_LABEL = "'friction.dat' Datei";

  private static final String FRIC_FILTER_LABEL = "SOBEK friction.dat File";

  private static final String DEF_EXTENSION = "def"; //$NON-NLS-1$

  private static final String FRIC_EXTENSION = "dat"; //$NON-NLS-1$

  private static final String SETTINGS_ID_PATTERN = "idPattern"; //$NON-NLS-1$

  private SobekFileChooser m_defFileChooser;

  private SobekFileChooser m_fricFileChooser;

  private String m_idPattern = "<Name>";

  public SobekProfileExportFileChooserPage( )
  {
    super( "sobekProfileExportFileChooserPage" ); //$NON-NLS-1$

    setTitle( "Exportdateien ausw‰hlen" );
    setDescription( "Bitte w‰hlen Sie Dateien aus, die exportiert werden sollen." );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite comp = new Composite( parent, SWT.NONE );
    comp.setLayout( new GridLayout() );

    createPatternControl( comp );

    final IDialogSettings defSettings = PluginUtilities.getSection( getDialogSettings(), "def" );
    final IDialogSettings fricSettings = PluginUtilities.getSection( getDialogSettings(), "dat" );
    m_defFileChooser = new SobekFileChooser( this, defSettings, DEF_FILTER_LABEL, DEF_EXTENSION );
    m_defFileChooser.createControl( comp, DEF_GROUP_LABEL );
    m_fricFileChooser = new SobekFricFileChooser( this, fricSettings, FRIC_FILTER_LABEL, FRIC_EXTENSION );
    m_fricFileChooser.createControl( comp, FRIC_GROUP_LABEL );

    setControl( comp );

    super.createControl( parent );
  }

  private void createPatternControl( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );
    panel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    new Label( panel, SWT.NONE ).setText( "ID-Pattern:" );

    final Text text = new Text( panel, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setText( m_idPattern );

    text.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleIdPatternChanged( text.getText() );
      }
    } );

    ProfilePatternInputReplacer.getINSTANCE().createPatternButton( panel, text );
  }

  protected void handleIdPatternChanged( final String currentValue )
  {
    m_idPattern = currentValue;

    updateMessage();

    final IDialogSettings dialogSettings = getDialogSettings();
    if( dialogSettings == null )
      return;

    dialogSettings.put( SETTINGS_ID_PATTERN, currentValue );
  }

  public ISobekProfileExportOperation[] getOperations( final IProfileFeature[] profiles )
  {
    final Collection<ISobekProfileExportOperation> ops = new ArrayList<ISobekProfileExportOperation>();

    final ISobekProfileExportOperation defOperation = m_defFileChooser.createOperation( profiles, m_idPattern );
    if( defOperation != null )
      ops.add( defOperation );

    final ISobekProfileExportOperation fricOperation = m_fricFileChooser.createOperation( profiles, m_idPattern );
    if( fricOperation != null )
      ops.add( fricOperation );

    return ops.toArray( new ISobekProfileExportOperation[ops.size()] );
  }

  @Override
  protected IMessageProvider validatePage( )
  {
    if( StringUtils.isBlank( m_idPattern ) )
      return new MessageProvider( "Bitte geben Sie ein das ID-Pattern ein", WARNING );

    if( m_defFileChooser != null )
    {
      final IMessageProvider defMessage = m_defFileChooser.validate();
      if( defMessage != null )
        return defMessage;
    }

    if( m_fricFileChooser != null )
      return m_fricFileChooser.validate();

    return null;
  }

}
