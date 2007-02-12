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
package org.kalypso.ui.wizard.roughness;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Vector;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;


public class ImportRoughessSourceWizardPage extends WizardPage
{

  private Composite topComposite;

  Text fileTextField;

  IProject m_project;

  IPath filePath;

  IPath stylePath;

  Text styleTextField;

  private Button browseButton1;

  Button browseButton2;

  Combo styleNameCombo;

  protected String styleName;

  boolean checkDefaultStyle = false;

  /*
   * @author peiler
   */
  protected ImportRoughessSourceWizardPage( String pageName, String title, ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
    setPageComplete( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    topComposite = new Composite( parent, SWT.NULL );
    topComposite.setFont( parent.getFont() );

    initializeDialogUnits( parent );

    topComposite.setLayout( new GridLayout() );
    topComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createFileGroup( topComposite );
    setControl( topComposite );
  }

  private void createFileGroup( Composite parent )
  {
    // data
    Group fileGroup = new Group( parent, SWT.NULL );
    fileGroup.setText( "RoughnessDataModel" );

    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    fileGroup.setLayoutData( data );
    GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    fileGroup.setLayout( gridLayout );

    Label fileLabel = new Label( fileGroup, SWT.NONE );
    fileLabel.setText( "Datei : " );

    fileTextField = new Text( fileGroup, SWT.BORDER );
    GridData data1 = new GridData();
    data1.horizontalAlignment = GridData.FILL;
    data1.grabExcessHorizontalSpace = true;
    fileTextField.setLayoutData( data1 );
    fileTextField.setEditable( false );

    browseButton1 = new Button( fileGroup, SWT.PUSH );
    browseButton1.setText( "Durchsuchen..." );
    browseButton1.setLayoutData( new GridData( GridData.END ) );
    browseButton1.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "gml" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path) result[0];
          fileTextField.setText( resultPath.toString() );
          filePath = resultPath;
        }
        validate();
      }
    } );

    // style
    Group styleGroup = new Group( parent, SWT.NULL );
    styleGroup.setText( "Style" );

    GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    styleGroup.setLayoutData( data3 );
    GridLayout gridLayout1 = new GridLayout();
    gridLayout1.numColumns = 3;
    styleGroup.setLayout( gridLayout1 );

    final Label styleLabel = new Label( styleGroup, SWT.NONE );
    styleLabel.setText( "Datei : " );

    styleTextField = new Text( styleGroup, SWT.BORDER );
    GridData data4 = new GridData();
    data4.horizontalAlignment = GridData.FILL;
    data4.grabExcessHorizontalSpace = true;
    styleTextField.setLayoutData( data4 );
    styleTextField.setEditable( false );

    browseButton2 = new Button( styleGroup, SWT.PUSH );
    browseButton2.setText( "Durchsuchen..." );
    browseButton2.setLayoutData( new GridData( GridData.END ) );
    browseButton2.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        KalypsoResourceSelectionDialog dialog = createResourceDialog( new String[] { "sld" } );
        dialog.open();
        Object[] result = dialog.getResult();
        if( result != null )
        {
          Path resultPath = (Path) result[0];
          styleTextField.setText( resultPath.toString() );
          stylePath = resultPath;
          Reader reader = null;
          try
          {
            final IPath basePath = m_project.getLocation();
            final String styleURLAsString = basePath.toFile().toURL() + stylePath.removeFirstSegments( 1 ).toString();
            final URL styleURL = new URL( styleURLAsString );
            reader = new InputStreamReader( (styleURL).openStream() );
            final IUrlResolver2 resolver = new IUrlResolver2()
            {

              public URL resolveURL( String href ) throws MalformedURLException
              {
                return UrlResolverSingleton.resolveUrl( styleURL, href );
              }

            };
            final StyledLayerDescriptor styledLayerDescriptor = SLDFactory.createSLD( resolver, reader );
            reader.close();
            final Layer[] layers = styledLayerDescriptor.getLayers();
            final Vector<String> styleNameVector = new Vector<String>();
            for( int i = 0; i < layers.length; i++ )
            {
              Layer layer = layers[i];
              Style[] styles = layer.getStyles();
              for( int j = 0; j < styles.length; j++ )
              {
                styleNameVector.add( styles[j].getName() );
              }
            }
            String[] styleNames = new String[styleNameVector.size()];
            for( int k = 0; k < styleNameVector.size(); k++ )
            {
              styleNames[k] = styleNameVector.get( k );
            }
            styleNameCombo.setItems( styleNames );
            styleNameCombo.select( 0 );
            styleName = styleNames[0];
          }
          catch( Exception e1 )
          {
            e1.printStackTrace();
          }
          finally
          {
            IOUtils.closeQuietly( reader );
          }
        }
        validate();
      }
    } );

    final Label styleNameLabel = new Label( styleGroup, SWT.NONE );
    styleNameLabel.setText( "UserStyle name: " );

    styleNameCombo = new Combo( styleGroup, SWT.READ_ONLY );
    GridData data5 = new GridData();
    data5.horizontalAlignment = GridData.FILL;
    data5.grabExcessHorizontalSpace = true;
    styleNameCombo.setLayoutData( data5 );
    styleNameCombo.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        styleName = styleNameCombo.getText();
      }
    } );

    Label dummyLabel = new Label( styleGroup, SWT.NONE );
    dummyLabel.setText( "" );

    final Button checkDefaultStyleButton = new Button( styleGroup, SWT.CHECK );
    checkDefaultStyleButton.setSelection( checkDefaultStyle );
    checkDefaultStyleButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        checkDefaultStyle = checkDefaultStyleButton.getSelection();
        if( checkDefaultStyleButton.getSelection() )
        {
          styleLabel.setEnabled( false );
          styleTextField.setEnabled( false );
          browseButton2.setEnabled( false );
          styleNameLabel.setEnabled( false );
          styleNameCombo.setEnabled( false );
        }
        else
        {
          styleLabel.setEnabled( true );
          styleTextField.setEnabled( true );
          browseButton2.setEnabled( true );
          styleNameLabel.setEnabled( true );
          styleNameCombo.setEnabled( true );
        }
        validate();
      }
    } );

    Label defaultStyleLabel = new Label( styleGroup, SWT.NONE );
    defaultStyleLabel.setText( "Generate default style" );

  }

  KalypsoResourceSelectionDialog createResourceDialog( String[] fileResourceExtensions )
  {
    return new KalypsoResourceSelectionDialog( getShell(), m_project, "Select resource", fileResourceExtensions, m_project, new ResourceSelectionValidator() );
  }

  public void setProject( IProject project )
  {
    m_project = project;
  }

  void validate( )
  {
    setErrorMessage( null );
    setMessage( null );
    StringBuffer error = new StringBuffer();
    if( filePath != null )
    {
      setPageComplete( true );
    }
    else
    {
      error.append( "Bitte RauheitsDataModel ausgew‰hlen.\n\n" );
      setPageComplete( false );
    }

    if( !checkDefaultStyle )
    {
      if( stylePath != null )
      {
        setPageComplete( true );
      }
      else
      {
        error.append( "Bitte Style ausgew‰hlen.\n" );
        setPageComplete( false );
      }

      if( styleName != null )
      {
        setPageComplete( true );
      }
      else
      {
        error.append( "Bitte Style name ausgew‰hlen.\n" );
        setPageComplete( false );
      }
    }

    if( error.length() > 0 )
      setErrorMessage( error.toString() );
    // setMessage( error.toString() );
    else
      setMessage( "Eingaben OK" );
  }

  public IPath getFilePath( )
  {
    return filePath;
  }

  public boolean checkDefaultStyle( )
  {
    return checkDefaultStyle;
  }

  public IPath getStylePath( )
  {
    return stylePath;
  }

  public String getStyleName( )
  {
    return styleName;
  }
}