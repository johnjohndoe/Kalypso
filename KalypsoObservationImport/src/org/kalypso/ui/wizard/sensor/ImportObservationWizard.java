package org.kalypso.ui.wizard.sensor;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.List;

import javax.xml.bind.Marshaller;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.adapter.INativeObservationAdapter;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.ObservationType;

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

public class ImportObservationWizard extends Wizard implements IImportWizard
{
  private ImportObservationSelectionWizardPage m_page1 = null;

  private IStructuredSelection m_selection;

  private ImportObservationAxisMappingWizardPage m_page2;

  public ImportObservationWizard()
  {
    super();
    setHelpAvailable( false );
    setNeedsProgressMonitor( false );
    setWindowTitle( "Import Observation" );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection currentSelection )
  {
    m_selection = currentSelection;
    final List selectedResources = IDE.computeSelectedResources( currentSelection );
    if( !selectedResources.isEmpty() )
    {
      m_selection = new StructuredSelection( selectedResources );
    }

    setWindowTitle( "Title" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();
    m_page2 = new ImportObservationAxisMappingWizardPage( "Analyse der Import-Datei" );

    m_page1 = new ImportObservationSelectionWizardPage( "Dateien waehlen" );
    addPage( m_page1 );
    addPage( m_page2 );

    m_page1.setSelection( m_selection );
    m_page1.addSelectionChangedListener( m_page2 );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performCancel()
   */
  public boolean performCancel()
  {
    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish()
  {
    try
    {
      final ObservationImportSelection selection = (ObservationImportSelection)m_page1.getSelection();
      final File fileSource = selection.getFileSource();
      final File fileTarget = selection.getFileTarget();
      final INativeObservationAdapter nativaAdapter = selection.getNativeAdapter();
      final IObservation srcObservation = nativaAdapter.createObservationFromSource( fileSource );

      final IAxis[] axesSrc = m_page2.getAxisMappingSrc();
      final IAxis[] axesNew = m_page2.getAxisMappingTarget();

      final ITuppleModel tuppelModelSrc = srcObservation.getValues( null );
      final int countSrc = tuppelModelSrc.getCount();

      final IObservation targetObservation;
      final ITuppleModel tuppelModelTarget;
      final int countTarget;
      if( fileTarget.exists() && ( selection.isAppend() || selection.isRetainMetadata() ) )
      {
        targetObservation = m_page2.getTargetObservation( fileTarget.toURL() );
        tuppelModelTarget = targetObservation.getValues( null );
        if( selection.isAppend() )
          countTarget = tuppelModelTarget.getCount();
        else
          countTarget = 0;
      }
      else
      {
        targetObservation = null;
        tuppelModelTarget = null;
        countTarget = 0;
      }
      // create new values
      final ITuppleModel newTuppelModel;
      if( tuppelModelTarget != null )
      {
        // w/q specials...
        if( tuppelModelTarget instanceof WQTuppleModel )
        {
          final WQTuppleModel wq = (WQTuppleModel)( tuppelModelTarget );
          final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length - 1];
          final ITuppleModel model = new SimpleTuppleModel( axesNew, newValues );
          newTuppelModel = new WQTuppleModel( model, axesNew, wq.getDateAxis(), wq.getSrcAxis(), wq.getSrcStatusAxis(),
              wq.getDestAxis(), wq.getDestStatusAxis(), wq.getConverter(), wq.getDestAxisPos(), wq
                  .getDestStatusAxisPos() );
        }
        else
        {
          final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length];
          newTuppelModel = new SimpleTuppleModel( axesNew, newValues );
        }
      }
      else
      {
        final Object[][] newValues = new Object[countSrc + countTarget][axesNew.length];
        newTuppelModel = new SimpleTuppleModel( axesNew, newValues );
      }
      // fill from source
      for( int i = 0; i < countSrc; i++ )
      {
        for( int a = 0; a < axesNew.length; a++ )
        {
          final Object newValue;
          if( axesSrc[a] == null )
          {
            if( KalypsoStatusUtils.isStatusAxis( axesNew[a] ) )
              newValue = new Integer( KalypsoStati.BIT_USER_MODIFIED );
            else
              newValue = null;
          }
          else
            newValue = tuppelModelSrc.getElement( i, axesSrc[a] );
          if( newValue != null )
            newTuppelModel.setElement( i, newValue, axesNew[a] );
        }
      }
      // append from existing target
      if( tuppelModelTarget != null )
      {
        for( int i = 0; i < countTarget; i++ )
          for( int a = 0; a < axesNew.length; a++ )
            newTuppelModel.setElement( countSrc + i, tuppelModelTarget.getElement( i, axesNew[a] ), axesNew[a] );
      }
      final String href = "";
      final String id = "";
      final String name = "";
      final MetadataList metadata = new MetadataList();
      if( targetObservation != null && selection.isRetainMetadata() )
        metadata.putAll( targetObservation.getMetadataList() );
      metadata.putAll( srcObservation.getMetadataList() );
      IObservation newObservation = new SimpleObservation( href, id, name, false, null, metadata, axesNew,
          newTuppelModel );
      ObservationType type = ZmlFactory.createXML( newObservation, null );
      // create new Observation...

      final ObjectFactory zmlFac = new ObjectFactory();

      final Marshaller marshaller = zmlFac.createMarshaller();
      // use IResource
      FileOutputStream stream = new FileOutputStream( new File( fileTarget.getPath() ) );
      OutputStreamWriter writer = new OutputStreamWriter( stream, "UTF-8" );
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      marshaller.marshal( type, writer );
      writer.close();
      // TODO refresh resources or use IResource
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }
}