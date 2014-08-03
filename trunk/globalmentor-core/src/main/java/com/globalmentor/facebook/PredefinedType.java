/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.facebook;

import static com.globalmentor.facebook.PredefinedCategory.*;

import com.globalmentor.model.IDed;

/**
 * One of the types predefined in the Open Graph specification.
 * @author Garret Wilson
 * @see <a href="http://ogp.me/">The Open Graph Protocol</a>
 */
public enum PredefinedType implements IDed<String> {
	//note: new predefined types must also be added to a category within getCategory() or an assertion exception will be thrown

	ACTIVITY, SPORT, BAR, COMPANY, CAFE, HOTEL, RESTAURANT, CAUSE, SPORTS_LEAGUE, SPORTS_TEAM, BAND, GOVERNMENT, NON_PROFIT, SCHOOL, UNIVERSITY, ACTOR, ATHLETE, AUTHOR, DIRECTOR, MUSICIAN, POLITICIAN, PROFILE, PUBLIC_FIGURE, CITY, COUNTRY, LANDMARK, STATE_PROVINCE, ALBUM, BOOK, DRINK, FOOD, GAME, MOVIE, PRODUCT, SONG, TV_SHOW, ARTICLE, BLOG, WEBSITE;

	/** @return The predefined category of this predefined type. */
	public PredefinedCategory getCategory() {
		switch(this) {
			case ACTIVITY:
			case SPORT:
				return ACTIVITIES;
			case BAR:
			case COMPANY:
			case CAFE:
			case HOTEL:
			case RESTAURANT:
				return BUSINESSES;
			case CAUSE:
			case SPORTS_LEAGUE:
			case SPORTS_TEAM:
				return GROUPS;
			case BAND:
			case GOVERNMENT:
			case NON_PROFIT:
			case SCHOOL:
			case UNIVERSITY:
				return ORGANIZATIONS;
			case ACTOR:
			case ATHLETE:
			case AUTHOR:
			case DIRECTOR:
			case MUSICIAN:
			case POLITICIAN:
			case PROFILE:
			case PUBLIC_FIGURE:
				return PEOPLE;
			case CITY:
			case COUNTRY:
			case LANDMARK:
			case STATE_PROVINCE:
				return PLACES;
			case ALBUM:
			case BOOK:
			case DRINK:
			case FOOD:
			case GAME:
			case MOVIE:
			case PRODUCT:
			case SONG:
			case TV_SHOW:
				return PRODUCTS_ENTERTAINMENT;
			case ARTICLE:
			case BLOG:
			case WEBSITE:
				return WEBSITES;
			default:
				throw new AssertionError("No category defined for type " + this);
		}
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This ID is the official ID given by the Open Graph specification and suitable for serialization.
	 * </p>
	 */
	public String getID() {
		return toString().toLowerCase(); //all the type IDs are simply the lowercase version of the enum string
	}
}
