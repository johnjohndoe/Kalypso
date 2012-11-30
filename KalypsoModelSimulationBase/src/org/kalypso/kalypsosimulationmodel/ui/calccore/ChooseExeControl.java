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
package org.kalypso.kalypsosimulationmodel.ui.calccore;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public class ChooseExeControl extends AbstractFeatureControl
{
  private final String m_exePattern;

  private final String m_displayName;

  private int m_alignment = SWT.CENTER;

  private Button m_button;

  private String m_displayFormat = Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.0" );//$NON-NLS-1$

  public ChooseExeControl( final Feature feature, final IPropertyType ftp, final String exePattern, final String displayName )
  {
    super( feature, ftp );

    m_exePattern = exePattern;
    m_displayName = displayName;
  }

  public void setDisplayFormat( final String displayFormat )
  {
    m_displayFormat = displayFormat;
  }

  public void setButtonAlignment( final int alignment )
  {
    m_alignment = alignment;
  }

  @Override
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );
    m_button.setAlignment( m_alignment );
    m_button.setText( "..." ); //$NON-NLS-1$
    m_button.setToolTipText( Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.0" ) ); //$NON-NLS-1$
    m_button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleButtonPressed( e );
      }
    } );

    updateControl();

    return m_button;
  }

  protected void handleButtonPressed( final SelectionEvent e )
  {
    final Shell shell = e.display.getActiveShell();

    // find all possible exes
    final Pattern pattern = Pattern.compile( m_exePattern, Pattern.CASE_INSENSITIVE );

    /* Always call this in order to provoke the download error message */
    final File[] exeFiles = CalcCoreUtils.checkExecutablesAvailable( shell, m_exePattern, m_displayFormat );
    if( exeFiles == null )
      return;

    // Find the currently chosen file, if any
    final String oldVersion = (String) getFeature().getProperty( getFeatureTypeProperty() );
    File selectedFile = null;
    for( final File file : exeFiles )
    {
      final String newVersion = versionFromFile( pattern, file );
      if( ObjectUtils.equals( oldVersion, newVersion ) )
      {
        selectedFile = file;
        break;
      }
    }

    // let user choose a new version/file
    final ChooseExeDialog dialog = new ChooseExeDialog( shell, exeFiles );
    dialog.setTitle( m_displayName );
    if( selectedFile != null )
      dialog.setInitialSelections( new File[] { selectedFile } );
    if( dialog.open() != Window.OK )
      return;

    final Object[] result = dialog.getResult();
    if( result.length == 0 )
      return;

    final File chosenFile = (File) result[0];
    final String newVersion = versionFromFile( pattern, chosenFile );

    final ChangeFeatureCommand command = new ChangeFeatureCommand( getFeature(), getFeatureTypeProperty(), newVersion );
    fireFeatureChange( command );
  }

  protected String versionFromFile( final Pattern pattern, final File file )
  {
    final Matcher matcher = pattern.matcher( file.getName() );
    matcher.matches();
    return matcher.group( 1 );
  }

  @Override
  public boolean isValid( )
  {
    return true;
  }

  @Override
  public void updateControl( )
  {
    final Feature feature = getFeature();
    final IPropertyType property = getFeatureTypeProperty();
    if( feature != null && property != null && m_displayFormat != null )
    {
      final String value = (String) feature.getProperty( property );
      if( StringUtils.isBlank( value ) )
        m_button.setText( Messages.getString("ChooseExeControl.0") ); //$NON-NLS-1$
      else
      {
        final String text = String.format( m_displayFormat, value );
        m_button.setText( text );
      }
    }
  }
}
