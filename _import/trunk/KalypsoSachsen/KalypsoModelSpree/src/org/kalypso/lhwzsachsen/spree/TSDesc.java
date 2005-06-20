package org.kalypso.lhwzsachsen.spree;

public final class TSDesc
{
  public final String id;

  public final boolean output;

  public final String useMetadataFrom;

  public TSDesc( final String id )
  {
    this( id, false, null );
  }

  public TSDesc( final String id, final boolean output, final String useMetadataFrom )
  {
    this.id = id;
    this.output = output;
    this.useMetadataFrom = useMetadataFrom;
  }
}