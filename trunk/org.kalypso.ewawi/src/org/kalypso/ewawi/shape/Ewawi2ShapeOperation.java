package org.kalypso.ewawi.shape;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.kalypso.ewawi.shape.writer.EwawiShape244Writer;
import org.kalypso.ewawi.shape.writer.EwawiShape32Writer;
import org.kalypso.ewawi.shape.writer.EwawiShape347Writer;
import org.kalypso.ewawi.shape.writer.EwawiShape348Writer;
import org.kalypso.ewawi.shape.writer.EwawiShape38Writer;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.reader.EwawiDirReader;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.shp.SHPException;

/**
 * @author Gernot Belger
 */
public class Ewawi2ShapeOperation implements IRunnableWithProgress
{
  private final Ewawi2ShapeData m_data;

  public Ewawi2ShapeOperation( final Ewawi2ShapeData data )
  {
    m_data = data;
  }

  @Override
  public void run( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    try
    {
      /* Get the ewawi plus objects. */
      final EwawiPlus[] ewawiData = readEwawiData();

      /* Read the gew shape. */
      final GewShape gewShape = readGewShape();

      /* Read the gew width shape. */
      final GewWidthShape gewWidthShape = readGewWidthShape();

      /* Write shape with ID 32. */
      final EwawiShape32Writer writer32 = new EwawiShape32Writer( ewawiData, gewShape, gewWidthShape );
      writer32.writeShape();

      /* Write shape with ID 38. */
      final EwawiShape38Writer writer38 = new EwawiShape38Writer( ewawiData, gewShape, gewWidthShape );
      writer38.writeShape();

      /* Write shape with ID 244. */
      final EwawiShape244Writer writer244 = new EwawiShape244Writer( ewawiData, gewShape, gewWidthShape );
      writer244.writeShape();

      /* Write shape with ID 347. */
      final EwawiShape347Writer writer347 = new EwawiShape347Writer( ewawiData, gewShape, gewWidthShape );
      writer347.writeShape();

      /* Write shape with ID 348. */
      final EwawiShape348Writer writer348 = new EwawiShape348Writer( ewawiData, gewShape, gewWidthShape );
      writer348.writeShape();
    }
    catch( DBaseException | IOException | SHPException | EwawiException e )
    {
      throw new InvocationTargetException( e );
    }
  }

  /**
   * Reads all ewawi files from the input dir. Each set of files with the same generated key are put together.
   */
  private EwawiPlus[] readEwawiData( )
  {
    final EwawiDirReader reader = new EwawiDirReader();
    reader.read( m_data.getInputDir() );
    return reader.getData();
  }

  private GewShape readGewShape( ) throws DBaseException, IOException
  {
    final GewShape gewShape = new GewShape( m_data.getGewShape() );
    gewShape.init();

    return gewShape;
  }

  private GewWidthShape readGewWidthShape( ) throws DBaseException, IOException
  {
    final GewWidthShape gewWidthShape = new GewWidthShape( m_data.getGewWidthShape() );
    gewWidthShape.init();

    return gewWidthShape;
  }
}