// NilController.cpp: Implementierung der Klasse CNilController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "FlipProfileController.h"
#include "mapHelper.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

CFlipProfileController::CFlipProfileController( CMapView* view ) : IMapController( view ) {};
CFlipProfileController::~CFlipProfileController() {};

void CFlipProfileController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
	BeepOnDestroy beep( true );
	
	CMapDoc* doc = m_view->GetDocument();
	
	CLayerArray* layers = doc->GetLayers();
	CMapLayer* lineLayer = layers->FindFirstLayer( CLayer::profilLines );
	CMapLayer* pointLayer = layers->FindFirstLayer( CLayer::profilPoints );
	CMapLayer* bezugLayer = layers->FindFirstLayer( CLayer::festpunkte );
	
	if( !lineLayer || !pointLayer )
		return;
	
	const int profilID = lineLayer->SearchNextObjectByDistance( cursor, m_view->GetSearchDistance());
	if( profilID == -1 )
		return;

	const OffsetAndNumber offsetAndNumber = CalcOffset( pointLayer, profilID );
	
	// für alle lineLayer:
	for( int i = 0; i < layers->GetSize(); i++ )
	{
		// alle punkte mit dieser id finden
		CLayer* lineLayer = layers->GetAt( i );
		if( lineLayer->GetLayerType() == moMapLayer )
		{
			CMapLayer* mapLayer = (CMapLayer*)lineLayer;
			CMoPoint nullPoint = FlipLayer( mapLayer, profilID, offsetAndNumber );
			if( LPDISPATCH(nullPoint) && bezugLayer )
				ChangeFixpoint( bezugLayer, nullPoint, profilID );
		}
		

		// refresh der karte
		//			m_view->GetDocument()->SetOverview( mapLayer->GetDispatch() );
	}
	// dont beep on success
	beep.setBeep( false );
	doc->FireMapDocChanged( IMapDocListener::THEME_DATA_CHANGED | IMapDocListener::THEME_GEOMETRY_CHANGED, 0 );
}

CMoPoint CFlipProfileController::FlipLayer( CMapLayer* layer, const long profilID, const OffsetAndNumber offsetAndNumber ) const
{
	CString searchExpr;
	searchExpr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
	CMoRecordset records( layer->SearchExpression( searchExpr ) );
				
	CMoFields fields( records.GetFields() );
	CMoField yField = fields.Item( CComVariant( MO2_FIELD_YKRD ) );
	CMoField nrField = fields.Item( CComVariant( MO2_FIELD_NUMBER ) );
	CMoField shapeField = fields.Item( CComVariant( MO2_FIELD_SHAPE ) );

	records.SetAutoFlush( FALSE );

	CMoPoint nullPoint;
	while( !records.GetEof() )
	{
		const double y = yField.GetValue().dblVal;
		const double newY = -y + offsetAndNumber.offset;

		if( fabs( newY ) < 0.0001 && layer->GetType() == CLayer::profilPoints )
		{
			LPDISPATCH disp = shapeField.GetValue().pdispVal;
			disp->AddRef();
			CMoPoint point( disp );
			
			point.GetX();
			point.GetY();

			MO2CREATE( nullPoint, "Point" );
			nullPoint.SetX( point.GetX() );
			nullPoint.SetY( point.GetY() );
		}
		
		records.Edit();

		yField.SetValue( CComVariant( newY ) );

		if( LPDISPATCH( nrField ) )
		{
			const long number = nrField.GetValue().lVal;
			const long newNr = offsetAndNumber.number - number;
			nrField.SetValue( CComVariant( newNr ) );
		}

		records.Update();
		records.MoveNext();
	}
	
	records.SetAutoFlush( TRUE );

	return nullPoint;
}

/**
 * Simply find the maximal yKrd of the given layer
 */
const CFlipProfileController::OffsetAndNumber CFlipProfileController::CalcOffset( CMapLayer* pointLayer, const long profilID ) const
{
	CString searchExpr;
	searchExpr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
	CMoRecordset records( pointLayer->SearchExpression( searchExpr ) );
				
	CMoFields fields( records.GetFields() );
	CMoField yField = fields.Item( CComVariant( MO2_FIELD_YKRD ) );
	CMoField nrField = fields.Item( CComVariant( MO2_FIELD_NUMBER ) );

	double offset = 0.0;
	long number = 0;
	while( !records.GetEof() )
	{
		const double y = yField.GetValue().dblVal;
		const long nr = nrField.GetValue().lVal;
		offset = max( offset, y );
		number = max( number, nr );

		records.MoveNext();
	}

	OffsetAndNumber oan;
	oan.offset = offset;
	oan.number = number;
	return oan;
}

void CFlipProfileController::ChangeFixpoint( CMapLayer* layer, CMoPoint point, const int profilID ) const
{
	CString searchExpr;
	searchExpr.Format( "%s = %d", MO2_FIELD_PROFILID, profilID );
	CMoRecordset records( layer->SearchExpression( searchExpr ) );
				
	CMoFields fields( records.GetFields() );
	CMoField shapeField = fields.Item( CComVariant( MO2_FIELD_SHAPE ) );
	if( records.GetEof() )
		return;

	// only change first point
	records.Edit();
	shapeField.SetValue( CComVariant( point ) );
	records.Update();
}