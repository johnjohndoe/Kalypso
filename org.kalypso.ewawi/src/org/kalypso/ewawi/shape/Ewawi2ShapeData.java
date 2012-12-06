package org.kalypso.ewawi.shape;

import java.io.File;

/**
 * (Input-)Data for {@link Ewawi2ShapeOperation}.
 * 
 * @author Gernot Belger
 */
public class Ewawi2ShapeData
{
  private final File m_inputDir;

  private final File m_gewShape;

  private final File m_gewWidthShape;

  public Ewawi2ShapeData( final File inputDir, final File gewShape, final File gewWidthShape )
  {
    m_inputDir = inputDir;
    m_gewShape = gewShape;
    m_gewWidthShape = gewWidthShape;
  }

  public File getInputDir( )
  {
    return m_inputDir;
  }

  public File getGewShape( )
  {
    return m_gewShape;
  }

  public File getGewWidthShape( )
  {
    return m_gewWidthShape;
  }
}