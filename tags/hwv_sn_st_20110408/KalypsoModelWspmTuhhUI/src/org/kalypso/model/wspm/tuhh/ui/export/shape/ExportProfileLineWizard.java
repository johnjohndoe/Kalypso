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
package org.kalypso.model.wspm.tuhh.ui.export.shape;

import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeOperation;
import org.kalypso.gml.ui.commands.exportshape.ExportShapePage;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.ProfileExportUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.ResultProfileInterpolator;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.csv.CsvExportColumnsPage;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.action.ProfileSelection;
import org.kalypso.model.wspm.ui.profil.wizard.results.IResultInterpolationSettings;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.deegree.IShapeDataFactory;

public class ExportProfileLineWizard extends ExportProfilesWizard
{
  private final ExportShapePage m_exportShapePage;

  private final CsvExportColumnsPage m_columnsPage;

  public ExportProfileLineWizard( final ProfileSelection selection, final String fileName )
  {
    super( selection );

    setShowResultInterpolationSettings( true );

    final IDialogSettings wizardSettings = PluginUtilities.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );
    setDialogSettings( wizardSettings );

    m_columnsPage = new CsvExportColumnsPage( selection );
    addPage( m_columnsPage );

    m_exportShapePage = new ExportShapePage( "exportShapePage", fileName ); //$NON-NLS-1$
    addPage( m_exportShapePage );

    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard#exportProfiles(org.kalypso.model.wspm.core.gml.IProfileFeature[],
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
  {
    final Charset shapeCharset = m_exportShapePage.getCharset();
    final String coordinateSystem = m_exportShapePage.getCoordinateSystem();
    final String shapeFileBase = m_exportShapePage.getShapeFileBase();
    final boolean doWritePrj = m_exportShapePage.isWritePrj();

    final PatternReplacementColumn[] exportColumns = m_columnsPage.getExportColumns();

    final IResultInterpolationSettings resultInterpolationSettings = getResultInterpolationSettings();
    final boolean doAdd = resultInterpolationSettings.shouldAddInterpolatedProfiles();
    final boolean interpolateForeland = resultInterpolationSettings.shouldInterpolateForland();

    final IWspmResult[] results = ProfileExportUtils.findResults( profiles, exportColumns );

    final ResultProfileInterpolator interpolator = new ResultProfileInterpolator( profiles, results, doAdd, interpolateForeland );
    final IProfileFeature[] interpolatedProfiles = interpolator.execute();

    final IDBFValue[] fields = fillMapping( exportColumns );

    final IShapeDataFactory shapeDataFactory = new ProfileLineDataFactory( interpolatedProfiles, shapeCharset, coordinateSystem, fields );

    try
    {
      final ICoreRunnableWithProgress operation = new ExportShapeOperation( shapeFileBase, shapeDataFactory, doWritePrj );
      operation.execute( monitor );
    }
    catch( final InvocationTargetException e )
    {
      final String msg = Messages.getString("ExportProfileLineWizard_1"); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), msg, e.getTargetException() );
      throw new CoreException( status );
    }
    catch( final InterruptedException e )
    {
      throw new CoreException( Status.CANCEL_STATUS );
    }
  }

  private IDBFValue[] fillMapping( final PatternReplacementColumn[] exportColumns )
  {
    final Collection<IDBFValue> fields = new ArrayList<IDBFValue>();
    try
    {
      for( final PatternReplacementColumn column : exportColumns )
      {
        final DBFField field = createField( column );
        fields.add( new PatternReplacementField( column, field ) );
      }
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
    }

    return fields.toArray( new IDBFValue[fields.size()] );
  }

  private static DBFField createField( final PatternReplacementColumn column ) throws DBaseException
  {
    final String name = column.getHeader();
    final int formatWidth = column.getFormatWidth();
    final int formatPrecision = column.getFormatPrecision();
    final FieldType type = findFieldType( column.getType() );

    final int width = formatWidth == -1 ? column.getDefaultWidth() : formatWidth;
    final int precision = formatPrecision == -1 ? column.getDefaultPrecision() : formatPrecision;

    return new DBFField( name, type, (short) width, (short) precision ); //$NON-NLS-1$
  }

  // FIXME: move into shape helper
  private static FieldType findFieldType( final Class< ? > type )
  {
    if( Boolean.class == type )
      return FieldType.L;

    if( Number.class.isAssignableFrom( type ) )
      return FieldType.N;

    if( Date.class == type || Calendar.class == type )
      return FieldType.D;

    return FieldType.C;
  }

}
