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
import org.kalypso.model.wspm.tuhh.core.profile.pattern.ProfilePatternInputReplacer;
import org.kalypso.model.wspm.tuhh.ui.export.ValidatingWizardPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class SobekProfileExportFileChooserPage extends ValidatingWizardPage
{
  private static final String DEF_GROUP_LABEL = "Profile.def"; //$NON-NLS-1$

  private static final String DEF_FILTER_LABEL = Messages.getString("SobekProfileExportFileChooserPage_1"); //$NON-NLS-1$

  private static final String FRIC_GROUP_LABEL = "Friction.dat"; //$NON-NLS-1$

  private static final String FRIC_FILTER_LABEL = Messages.getString("SobekProfileExportFileChooserPage_3"); //$NON-NLS-1$

  private static final String STRUCT_GROUP_LABEL = "Struct.def"; //$NON-NLS-1$

  private static final String STRUCT_FILTER_LABEL = "SOBEK struct.def file";

  private static final String DEF_EXTENSION = "def"; //$NON-NLS-1$

  private static final String FRIC_EXTENSION = "dat"; //$NON-NLS-1$

  private static final String STRUCT_EXTENSION = "def"; //$NON-NLS-1$

  private static final String SETTINGS_ID_PATTERN = "idPattern"; //$NON-NLS-1$

  private static final String SETTINGS_BUILDINGS_SUFFIX = "buildingsSuffix"; //$NON-NLS-1$

  private static final String STR_SUFFIX_TOOLTIP = "Append this suffix to the id of structures";

  private String m_idSuffix = "_building";

  private AbstractSobekFileChooser m_defFileChooser;

  private AbstractSobekFileChooser m_fricFileChooser;

  private String m_idPattern = "<Name>"; //$NON-NLS-1$

  private SobekStructFileChooser m_structFileChooser;

  public SobekProfileExportFileChooserPage( )
  {
    super( "sobekProfileExportFileChooserPage" ); //$NON-NLS-1$

    setTitle( Messages.getString("SobekProfileExportFileChooserPage_5") ); //$NON-NLS-1$
    setDescription( Messages.getString("SobekProfileExportFileChooserPage_6") ); //$NON-NLS-1$

  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite parent )
  {
    readSettings();

    final Composite comp = new Composite( parent, SWT.NONE );
    comp.setLayout( new GridLayout() );

    final Composite idPanel = new Composite( comp, SWT.NONE );
    idPanel.setLayout( new GridLayout( 3, false ) );
    idPanel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    createPatternControl( idPanel );
    createBuildingSuffixControl( idPanel );

    final IDialogSettings defSettings = PluginUtilities.getSection( getDialogSettings(), "def" ); //$NON-NLS-1$
    final IDialogSettings fricSettings = PluginUtilities.getSection( getDialogSettings(), "dat" ); //$NON-NLS-1$
    final IDialogSettings structSettings = PluginUtilities.getSection( getDialogSettings(), "structDef" ); //$NON-NLS-1$

    m_defFileChooser = new SobekProfileFileChooser( this, defSettings, DEF_FILTER_LABEL, DEF_EXTENSION );
    m_defFileChooser.createControl( comp, DEF_GROUP_LABEL );

    m_fricFileChooser = new SobekFricFileChooser( this, fricSettings, FRIC_FILTER_LABEL, FRIC_EXTENSION );
    m_fricFileChooser.createControl( comp, FRIC_GROUP_LABEL );

    m_structFileChooser = new SobekStructFileChooser( this, structSettings, STRUCT_FILTER_LABEL, STRUCT_EXTENSION );
    m_structFileChooser.createControl( comp, STRUCT_GROUP_LABEL );

    setControl( comp );

    super.createControl( parent );
  }

  private void readSettings( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    final String idPattern = settings.get( SETTINGS_ID_PATTERN );
    if( idPattern != null )
      m_idPattern = idPattern;

    final String idSuffix = settings.get( SETTINGS_BUILDINGS_SUFFIX );
    if( idSuffix != null )
      m_idSuffix = idSuffix;
  }

  private void createPatternControl( final Composite parent )
  {
    final Label patternLabel = new Label( parent, SWT.NONE );
    patternLabel.setText( Messages.getString( "SobekProfileExportFileChooserPage_9" ) ); //$NON-NLS-1$
    patternLabel.setToolTipText( "This pattern is used to generate sobek profile ID's" );

    final Text text = new Text( parent, SWT.BORDER );
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

    ProfilePatternInputReplacer.getINSTANCE().createPatternButton( parent, text );
  }

  private void createBuildingSuffixControl( final Composite parent )
  {
    final Label suffixLabel = new Label( parent, SWT.NONE );
    suffixLabel.setText( "ID-Suffix" );
    suffixLabel.setToolTipText( STR_SUFFIX_TOOLTIP );

    final Text suffixText = new Text( parent, SWT.SINGLE | SWT.BORDER );
    suffixText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );
    suffixText.setText( m_idSuffix );
    suffixText.setToolTipText( STR_SUFFIX_TOOLTIP );

    suffixText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        handleSuffixModified( suffixText.getText() );
      }
    } );
  }

  protected void handleSuffixModified( final String idSuffix )
  {
    m_idSuffix = idSuffix;

    saveSettings();

    updateMessage();
  }

  private void saveSettings( )
  {
    final IDialogSettings settings = getDialogSettings();
    if( settings == null )
      return;

    settings.put( SETTINGS_BUILDINGS_SUFFIX, m_idSuffix );
    settings.put( SETTINGS_ID_PATTERN, m_idPattern );
  }

  protected void handleIdPatternChanged( final String currentValue )
  {
    m_idPattern = currentValue;

    updateMessage();

    saveSettings();

  }

  public ISobekProfileExportOperation[] getOperations( final IProfileFeature[] profiles )
  {
    final Collection<ISobekProfileExportOperation> ops = new ArrayList<ISobekProfileExportOperation>();

    final ISobekProfileExportOperation defOperation = m_defFileChooser.createOperation( profiles, m_idPattern, m_idSuffix );
    if( defOperation != null )
      ops.add( defOperation );

    final ISobekProfileExportOperation fricOperation = m_fricFileChooser.createOperation( profiles, m_idPattern, m_idSuffix );
    if( fricOperation != null )
      ops.add( fricOperation );

    final ISobekProfileExportOperation structOperation = m_structFileChooser.createOperation( profiles, m_idPattern, m_idSuffix );
    if( structOperation != null )
      ops.add( structOperation );

    return ops.toArray( new ISobekProfileExportOperation[ops.size()] );
  }

  @Override
  protected IMessageProvider validatePage( )
  {
    if( StringUtils.isBlank( m_idPattern ) )
      return new MessageProvider( Messages.getString("SobekProfileExportFileChooserPage_10"), WARNING ); //$NON-NLS-1$

    if( StringUtils.isBlank( m_idSuffix ) )
      return new MessageProvider( "Empty id-suffix for building - you will get id conflicts in the export file", IMessageProvider.WARNING );

    if( m_defFileChooser != null )
    {
      final IMessageProvider defMessage = m_defFileChooser.validate();
      if( defMessage != null )
        return defMessage;
    }

    if( m_fricFileChooser != null )
    {
      final IMessageProvider frictMessage = m_fricFileChooser.validate();
      if( frictMessage != null )
        return frictMessage;
    }

    if( m_structFileChooser != null )
      return m_structFileChooser.validate();

    return null;
  }

}
