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

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeOperation;
import org.kalypso.gml.ui.commands.exportshape.ExportShapePage;
import org.kalypso.gml.ui.commands.exportshape.ExportShapeUtils;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.profile.export.PatternReplacementColumn;
import org.kalypso.model.wspm.tuhh.core.profile.export.ProfileExportUtils;
import org.kalypso.model.wspm.tuhh.core.profile.export.ResultProfileInterpolator;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.export.ExportProfilesWizard;
import org.kalypso.model.wspm.tuhh.ui.export.csv.CsvExportColumnsPage;
import org.kalypso.model.wspm.ui.action.ProfilesSelection;
import org.kalypso.model.wspm.ui.profil.wizard.results.IResultInterpolationSettings;
import org.kalypso.shape.dbf.DBFField;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.dbf.IDBFValue;
import org.kalypso.shape.deegree.IShapeDataFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class ExportProfileLineWizard extends ExportProfilesWizard
{
  private ExportShapePage m_exportShapePage;

  private CsvExportColumnsPage m_columnsPage;

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    super.init( workbench, selection );

    final ProfilesSelection profileSelection = getProfileSelection();

    final String fileName = getFilename( profileSelection, selection );

    setShowResultInterpolationSettings( true );

    final IDialogSettings wizardSettings = DialogSettingsUtils.getDialogSettings( KalypsoModelWspmTuhhUIPlugin.getDefault(), getClass().getName() );
    setDialogSettings( wizardSettings );

    m_columnsPage = new CsvExportColumnsPage( profileSelection );
    addPage( m_columnsPage );

    m_exportShapePage = new ExportShapePage( "exportShapePage", fileName ); //$NON-NLS-1$
    addPage( m_exportShapePage );

    setNeedsProgressMonitor( true );
  }

  private String getFilename( final ProfilesSelection profileSelection, final ISelection selection )
  {
    final String fileName = ExportShapeUtils.guessExportFileName( selection );
    if( !StringUtils.isEmpty( fileName ) )
      return fileName;

    // if no theme, we should use the container.
    final Feature container = profileSelection.getContainer();
    if( container != null )
      return FeatureHelper.getAnnotationValue( container, IAnnotation.ANNO_LABEL );

    final IProfileFeature[] profiles = profileSelection.getProfiles();
    if( !ArrayUtils.isEmpty( profiles ) )
      return profiles[0].getName();

    return null;
  }

  @Override
  protected IStatus exportProfiles( final IProfileFeature[] profiles, final IProgressMonitor monitor ) throws CoreException
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

    final ExportShapeOperation operation = new ExportShapeOperation( shapeFileBase, shapeDataFactory, doWritePrj );
    return operation.execute( monitor );
  }

  private IDBFValue[] fillMapping( final PatternReplacementColumn[] exportColumns )
  {
    final Collection<IDBFValue> fields = new ArrayList<>();
    try
    {
      for( final PatternReplacementColumn column : exportColumns )
      {
        final IDBFField field = createField( column );
        fields.add( new PatternReplacementField( column, field ) );
      }
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
    }

    return fields.toArray( new IDBFValue[fields.size()] );
  }

  private static IDBFField createField( final PatternReplacementColumn column ) throws DBaseException
  {
    final String name = column.getHeader();
    final int formatWidth = column.getFormatWidth();
    final int formatPrecision = column.getFormatPrecision();
    final Class< ? > columnType = column.getType();
    final FieldType type = findFieldType( columnType );

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