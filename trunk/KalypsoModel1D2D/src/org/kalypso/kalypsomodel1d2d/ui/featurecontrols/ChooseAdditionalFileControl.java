/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.io.File;
import java.io.FilenameFilter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 * 
 */
public class ChooseAdditionalFileControl extends AbstractFeatureControl implements IFeatureControl
{
  private File m_file;

  private final File m_dir;

  private final String m_fileExt;

  public ChooseAdditionalFileControl( final Feature feature, final IPropertyType ftp, final File dir, final String fileExt )
  {
    super( feature, ftp );
    m_dir = dir;
    m_fileExt = fileExt;
    m_file = new File( (String) feature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_INPUT_COORD_FILE ) );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    final Button button = new Button( parent, style );
    button.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ChooseExeControl.0") ); //$NON-NLS-1$
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleButtonPressed( e );
      }
    } );

    return button;
  }

  /**
   * Answer the source file location or <code>null</code> if unspecified
   */
  public IPath getSourceLocation( )
  {
    final String text = m_file == null? "": m_file.getAbsoluteFile().toString();
    if( text.length() == 0 )
      return null;
    final IPath path = new Path( text );
    if( !path.isAbsolute() )
      return ResourcesPlugin.getWorkspace().getRoot().getLocation().append( path );
    else
      return path;
  }
  
  /**
   * Open a file browser dialog to locate a source file
   */
  protected void browseForSourceFile( final SelectionEvent e )
  {
    final Shell shell = e.display.getActiveShell();
    IPath path = browse( shell, getSourceLocation() );
    if( path == null )
      return;
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    m_file = new File( path.toString() );

  }

  private IPath browse( final Shell shell, final IPath path )
  {
    final FileDialog dialog = new FileDialog( shell, SWT.OPEN );
    dialog.setFilterExtensions( new String[]{ m_fileExt } ); //$NON-NLS-1$
    if( path != null )
    {
      if( path.segmentCount() > 1 )
        dialog.setFilterPath( path.removeLastSegments( 1 ).toOSString() );
      if( path.segmentCount() > 0 )
        dialog.setFileName( path.lastSegment() );
    }
    final String result = dialog.open();
    if( result == null )
      return null;
    return new Path( result );
  }

  
  protected void handleButtonPressed( final SelectionEvent e )
  {
    browseForSourceFile( e );
//    final Shell shell = e.display.getActiveShell();
//
//    // find all possible 
//    final Pattern pattern = Pattern.compile( m_fileExt, Pattern.CASE_INSENSITIVE );
//    final File[] exeFiles = m_dir.listFiles( new FilenameFilter()
//    {
//      public boolean accept( final File dir, final String name )
//      {
//        return pattern.matcher( name ).matches();
//      }
//    } );
//
//    if( exeFiles == null || exeFiles.length == 0 )
//    {
//      final String msg = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ChooseExeControl.1", m_dir.getAbsolutePath(), m_fileExt ); //$NON-NLS-1$
//      MessageDialog.openWarning( shell, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ChooseExeControl.2"), msg ); //$NON-NLS-1$
//      return;
//    }
//
//    // Find the currently chosen file, if any
//    final String oldVersion = (String) getFeature().getProperty( getFeatureTypeProperty() );
//    File selectedFile = null;
//    for( final File file : exeFiles )
//    {
//      final String newVersion = versionFromFile( pattern, file );
//      if( ObjectUtils.equals( oldVersion, newVersion ) )
//      {
//        selectedFile = file;
//        break;
//      }
//    }
//
//    // let user choose a new version/file
//    final ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell, new LabelProvider()
//    {
//      /**
//       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
//       */
//      @Override
//      public String getText( Object element )
//      {
//        return versionFromFile( pattern, (File) element );
//      }
//    } );
//    dialog.setMultipleSelection( false );
//    dialog.setAllowDuplicates( false );
//    dialog.setElements( exeFiles );
//    dialog.setMessage( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ChooseExeControl.3") ); //$NON-NLS-1$
//    dialog.setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ChooseExeControl.4") ); //$NON-NLS-1$
//    if( selectedFile != null )
//      dialog.setInitialSelections( new File[] { selectedFile } );
//    if( dialog.open() != Window.OK )
//      return;
//
//    final Object[] result = dialog.getResult();
//    if( result.length == 0 )
//      return;
//
//    final File chosenFile = (File) result[0];
    final String newVersion = m_file.getAbsolutePath(); 

    final ChangeFeatureCommand command = new ChangeFeatureCommand( getFeature(), getFeatureTypeProperty(), newVersion );
    fireFeatureChange( command );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
  }

}
