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
package org.kalypso.ui.views.properties;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.dialogs.PropertyPage;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.GisTemplateFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;

/**
 * This page shows the link to the data file of a {@link org.kalypso.ogc.gml.GisTemplateFeatureTheme}<br>
 * 
 * @author Gernot Belger
 */
public class FeatureThemePropertyPage extends PropertyPage implements IWorkbenchPropertyPage
{
  private String m_themeLocation = null;

  private String m_themeType = null;

  private String m_themeFeaturePath;

  /**
   * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    /* Get the theme. */
    final GisTemplateFeatureTheme theme = getTheme();
    final IPoolableObjectType layerKey = theme.getLayerKey();

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    if( theme == null )
    {
      // todo: show some error message
      return composite;
    }

    final String themeLocation = layerKey.getLocation();
    final String themeType = layerKey.getType();
    final String themeFeaturePath = theme.getFeaturePath();
    final IFeatureType featureType = theme.getFeatureType();

    /* Location */
    final Label locationLabel = new Label( composite, SWT.NONE );
    locationLabel.setText( Messages.getString( "org.kalypso.ui.views.properties.FeatureThemePropertyPage.locationLabel" ) ); //$NON-NLS-1$

    // Yet read only, as Feature-Theme does not support changing the location yet
    final Text sourceText = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    final GridData locationData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    locationData.widthHint = 200;
    sourceText.setLayoutData( locationData );
    sourceText.setText( themeLocation );
    sourceText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String location = sourceText.getText();
        setThemeLocation( location );
      }
    } );

    /* Location */
    final Label typeLabel = new Label( composite, SWT.NONE );
    typeLabel.setText( Messages.getString( "org.kalypso.ui.views.properties.FeatureThemePropertyPage.typeLabel" ) ); //$NON-NLS-1$

    // Yet read only, as Feature-Theme does not support changing the type yet
    final Text typeText = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    final GridData typeData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    typeData.widthHint = 200;
    typeText.setLayoutData( typeData );
    typeText.setText( themeType.toUpperCase() );
    typeText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String type = typeText.getText();
        setThemeType( type );
      }
    } );

    /* FeaturePath */
    final Label fpathLabel = new Label( composite, SWT.NONE );
    final GridData fpathLabelData = new GridData();
    fpathLabel.setLayoutData( fpathLabelData );
    fpathLabel.setText( Messages.getString( "org.kalypso.ui.views.properties.FeatureThemePropertyPage.fpathLabel" ) ); //$NON-NLS-1$

    // Yet read only, as Feature-Theme does not support changing the fpath yet
    final Text fpathText = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    final GridData fpathData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    fpathData.widthHint = 200;
    fpathText.setLayoutData( fpathData );
    fpathText.setText( themeFeaturePath );
    fpathText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String fpath = fpathText.getText();
        setThemeFeaturePath( fpath );
      }
    } );

    /* FeatureType */
    final Label ftypeLabel = new Label( composite, SWT.NONE );
    final GridData ftypeLabelData = new GridData();
    ftypeLabel.setLayoutData( ftypeLabelData );
    ftypeLabel.setText( Messages.getString( "org.kalypso.ui.views.properties.FeatureThemePropertyPage.ftypeLabel" ) ); //$NON-NLS-1$

    // Read only, as FeatureType cannot be changed, it is determined by the path
    final Text ftypeText = new Text( composite, SWT.READ_ONLY | SWT.BORDER );
    final GridData ftypeData = new GridData( SWT.FILL, SWT.CENTER, true, false );
    ftypeData.widthHint = 200;
    ftypeText.setLayoutData( ftypeData );
    ftypeText.setText( featureType.getQName().toString() );


    /* Hide things, that make no sense for shapes */
    if( themeType.equals( "shape" ) )
    {
      fpathLabelData.exclude = true;
      fpathData.exclude = true;
      ftypeLabelData.exclude = true;
      ftypeData.exclude = true;
    }

    return composite;
  }


  /**
   * This function returns the theme.
   * 
   * @return The theme.
   */
  private GisTemplateFeatureTheme getTheme( )
  {
    final IAdaptable element = getElement();
    final GisTemplateFeatureTheme theme = (GisTemplateFeatureTheme) (element instanceof GisTemplateFeatureTheme ? element : element.getAdapter( GisTemplateFeatureTheme.class ));
    return theme;
  }

  /**
   * @see org.eclipse.jface.preference.PreferencePage#performOk()
   */
  @Override
  public boolean performOk( )
  {
    final GisTemplateFeatureTheme theme = getTheme();
    if( theme == null )
      return false;

    final CompositeCommand allCommands = new CompositeCommand( Messages.getString( "org.kalypso.ui.views.properties.ThemePropertyPage.1" ) ); //$NON-NLS-1$

    // TODO: we do not know yet how to change the source
    try
    {
      allCommands.process();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return super.performOk();
  }

  private void checkValid( )
  {
    /* clear error */
    setErrorMessage( null );

    /* Revalidate and set message according to result */
    final String msg = validate();
    if( msg == null )
      setValid( true );
    else if( msg.length() == 0 )
      setValid( false );
    else
    {
      setValid( false );
      setErrorMessage( msg );
    }
  }

  private String validate( )
  {
    // If nothing changed, not valid, but also no message
    if( m_themeLocation == null && m_themeType == null && m_themeFeaturePath == null )
      return "";

    // TODO: check validity of data; reuse code of GisTemplate theme functionality (refaktor!)

    // everything else is valid
    return null;
  }

  protected void setThemeLocation( final String location )
  {
    m_themeLocation = location;

    checkValid();
  }

  protected void setThemeType( final String type )
  {
    m_themeType = type;

    checkValid();
  }

  protected void setThemeFeaturePath( final String fpath )
  {
    m_themeFeaturePath = fpath;

    checkValid();
  }
}
