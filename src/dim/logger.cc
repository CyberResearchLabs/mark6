/*
 * Created by David Lapsley on Mon Jun 6 2011.
 *
 * Copyright 2011 MIT Haystack Observatory 
 *  
 * This file is part of mark6.
 *
 * mark6 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * mark6 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with mark6.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

// Local includes.
#include <logger.h>

const std::string LOGGER_NAME("dim6");
const std::string DEFAULT_LOG_CONFIG("dim6-log.cfg");

// Global logger definition.
LoggerPtr logger(Logger::getLogger(LOGGER_NAME));

void init_logger(const std::string log_config)
{
  // Configure log subsystem.
  PropertyConfigurator::configure(log_config);
}

